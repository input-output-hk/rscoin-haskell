module Main where

import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Acid           (update, createCheckpoint)
import           Data.Text           as T

import qualified RSCoin.Bank         as B
import           RSCoin.Core         (Mintette (Mintette), Severity (Debug),
                                      constructPublicKey, defaultAccountsNumber,
                                      initLogging, logWarning, readPublicKey,
                                      readSecretKey)
import qualified RSCoin.Mintette     as M
import qualified RSCoin.User.AcidState as A
import RSCoin.User.Commands (UserCommand (..), proceedCommand)
import qualified RSCoin.User.Wallet    as W

import           RSCoin.Timed         (MsgPackRpc, bracket', runRealMode, fork)

import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Exception   (bracket_)

import           System.FilePath     ((</>))

import           Control.Monad.Catch   (MonadCatch, catch, throwM)
import System.IO.Temp (withSystemTempDirectory)

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
    let m  = Mintette "bench-mintette-1" 1234
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

userThread :: FilePath -> IO ()
userThread benchDir = runRealMode $ bracket'
    (liftIO $ A.openState $ benchDir </> "wallet-db")
    (\st -> liftIO $ do
        createCheckpoint st
        A.closeState st)
    (\st -> do 
        handleUninitialized
            (proceedCommand st UpdateBlockchain)
            (A.initState
                st
                defaultAccountsNumber
                Nothing
                {-(Just bankKeyFilePath)-}))

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
    putStrLn ">>> Starting bank"
    threadDelay (3^7)

    _ <- forkIO $ mintetteThread benchDir bankKeyFilePath
    putStrLn ">>> Starting mintette"
    threadDelay (5^7)
    
    _ <- forkIO $ userThread benchDir
    putStrLn ">>> Starting user"
    threadDelay (5^7)
