module Main where

import           Control.Lens           ((^.))
import           Control.Monad.Trans    (MonadIO, liftIO)
import           Data.Acid              (createCheckpoint, query, update)
import           Data.Int               (Int64)
import qualified Data.Text              as T
import           Data.Text.Lazy         (toStrict)
import           Formatting             (format, build)

import qualified RSCoin.Bank            as B
import           RSCoin.Core            (Mintette (Mintette), Severity (Debug),
                                         constructPublicKey,
                                         defaultAccountsNumber, initLogging,
                                         logWarning, readPublicKey,
                                         readSecretKey)
import qualified RSCoin.Mintette        as M
import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Commands   (UserCommand (..), proceedCommand)
import           RSCoin.User.Operations (getAmount, formTransaction)
import qualified RSCoin.User.Wallet     as W

import           RSCoin.Timed           (MsgPackRpc, bracket', fork,
                                         runRealMode)

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Exception      (bracket_)

import           System.FilePath        ((</>))

import           Control.Monad.Catch    (MonadCatch, catch, throwM)
import           System.IO.Temp         (withSystemTempDirectory)

tempBenchDirectory :: FilePath
tempBenchDirectory = ".bench-local"

defaultBankKey :: String
defaultBankKey = "SecKey \"448d85e1261c2ce919bdbdf1b3830653e91380f4f22ef6d5b0edfb6537dd0772\""

bankBracket :: FilePath -> (B.State -> MsgPackRpc ()) -> IO ()
bankBracket benchDir bankStateFun =
    runRealMode $ bracket'
        (liftIO $ B.openState $ benchDir </> "bank-db")
        (liftIO . B.closeState)
        bankStateFun

addMintette :: FilePath -> IO ()
addMintette benchDir = bankBracket benchDir $ \st -> liftIO $ do
    let m  = Mintette "127.0.0.1" 1234
    let pk = "A7DUEZGbDSAO4ruo8BWJDGyAioio7HFlLnDc5yPZRTz4"  -- TODO: generate new keys
    k     <- maybe (readPublicKeyFallback pk) return $ constructPublicKey pk
    update st $ B.AddMintette m k
  where
    readPublicKeyFallback pk = liftIO $ do
        logWarning "Failed to parse public key, trying to interpret as a filepath to key"
        readPublicKey $ T.unpack pk

bankThread :: FilePath -> FilePath -> IO ()
bankThread benchDir bankKeyFilePath = bankBracket benchDir $ \st -> do
    secretKey <- liftIO $ readSecretKey bankKeyFilePath
    fork $ B.runWorker secretKey st
    B.serve st

mintetteThread :: FilePath -> FilePath -> IO ()
mintetteThread benchDir bankKeyFilePath = do
    initLogging Debug
    secretKey <- readSecretKey bankKeyFilePath
    runRealMode $
        bracket'
            (liftIO $ M.openState $ benchDir </> "mintette-db")
            (liftIO . M.closeState) $
            \st -> do
                secretKey <- liftIO $ readSecretKey bankKeyFilePath
                fork $ M.runWorker secretKey st
                M.serve 1234 st secretKey

userThread :: Int64 -> FilePath -> FilePath -> IO ()
userThread userId benchDir bankKeyFilePath = runRealMode $ bracket'
    (liftIO $ A.openState $ benchDir </> "wallet-db" ++ show userId)
    (\st -> liftIO $ do
        createCheckpoint st
        A.closeState st)
    (\st -> do
        handleUninitialized
            (proceedCommand st UpdateBlockchain)
            (A.initState
                st
                defaultAccountsNumber
                (Just bankKeyFilePath)
            )

        -- show bank coin money
        adresses <- liftIO $ query st A.GetAllAddresses
        bankAmount <- getAmount st $ adresses !! 0
        liftIO $ putStrLn $ "Bank " ++ show userId ++ " amount: " ++ show bankAmount

        let bAdress = (adresses !! 1) ^. W.publicAddress
        proceedCommand st $ FormTransaction [(1, 50 + userId)] $ toStrict $ format build bAdress
        proceedCommand st UpdateBlockchain
        userAmount <- getAmount st $ adresses !! 1
        liftIO $ putStrLn $ "Adress " ++ show userId ++ " amount: " ++ show userAmount
    )
  where
    handleUninitialized :: (MonadIO m, MonadCatch m) => m () -> m () -> m ()
    handleUninitialized action initialize =
        action `catch` handler initialize action
    handler i a W.NotInitialized =
        liftIO (putStrLn "Initializing storage..") >> i >> a
    handler _ _ e = throwM e

main :: IO ()
main = withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
    let bankKeyFilePath = benchDir </> "rscoin-key"
    writeFile bankKeyFilePath defaultBankKey

    initLogging Debug

    addMintette benchDir

    _ <- forkIO $ bankThread benchDir bankKeyFilePath
    threadDelay (3 * 10^6)

    _ <- forkIO $ mintetteThread benchDir bankKeyFilePath
    threadDelay (3 * 10^6)

    _ <- forkIO $ userThread 1 benchDir bankKeyFilePath
    threadDelay (10 * 10^6)
    _ <- forkIO $ userThread 2 benchDir bankKeyFilePath
    threadDelay (10 * 10^6)
