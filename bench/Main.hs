{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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

import           Control.Monad.Extra (whenM)

import           System.Directory    (createDirectoryIfMissing,
                                      doesDirectoryExist,
                                      removeDirectoryRecursive)
import           System.FilePath     ((</>))

import           Control.Monad.Catch   (MonadCatch, catch, throwM)

tempBenchDirectory :: FilePath
tempBenchDirectory = ".bench-local"

bankKeyFilePath :: FilePath
bankKeyFilePath = tempBenchDirectory </> "rscoin-key"

bankLocalDirectory :: FilePath
bankLocalDirectory = tempBenchDirectory </> "bank-db"

walletLocalPath :: FilePath
walletLocalPath = tempBenchDirectory </> "wallet-db"

defaultBankKey :: String
defaultBankKey = "SecKey \"448d85e1261c2ce919bdbdf1b3830653e91380f4f22ef6d5b0edfb6537dd0772\""

setupBench :: IO ()
setupBench = do
    createDirectoryIfMissing False tempBenchDirectory
    writeFile bankKeyFilePath defaultBankKey

cleanupBench :: IO ()
cleanupBench = whenM (doesDirectoryExist tempBenchDirectory) (removeDirectoryRecursive tempBenchDirectory)
        
bankBracket :: (B.State -> MsgPackRpc ()) -> IO ()
bankBracket bankStateFun = 
    runRealMode $ bracket'
        (liftIO $ B.openState bankLocalDirectory)
        (liftIO . B.closeState)
        bankStateFun

addMintette :: IO ()
addMintette = bankBracket $ \st -> liftIO $ do
    let m  = Mintette "bench-mintette-1" 1234
    let pk = "A7DUEZGbDSAO4ruo8BWJDGyAioio7HFlLnDc5yPZRTz4"  -- TODO: generate new keys 
    k     <- maybe (readPublicKeyFallback pk) return $ constructPublicKey pk
    update st $ B.AddMintette m k
  where
    readPublicKeyFallback pk = liftIO $ do
        logWarning "Failed to parse public key, trying to interpret as a filepath to key"
        readPublicKey $ T.unpack pk

bankThread :: IO ()
bankThread = bankBracket $ \st -> do
    secretKey <- liftIO $ readSecretKey bankKeyFilePath
    fork $ B.runWorker secretKey st
    B.serve st

mintetteThread :: IO ()
mintetteThread = do
    initLogging Debug
    secretKey <- readSecretKey bankKeyFilePath
    runRealMode $
        bracket' 
            (liftIO $ M.openState $ tempBenchDirectory </> "mintette-db") 
            (liftIO . M.closeState) $
            \st -> do
                secretKey <- liftIO $ readSecretKey bankKeyFilePath
                fork $ M.runWorker secretKey st
                M.serve 1234 st secretKey

userThread :: IO ()
userThread = runRealMode $ bracket'
    (liftIO $ A.openState walletLocalPath)
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
main = bracket_ setupBench cleanupBench $ do
    initLogging Debug

    addMintette

    _ <- forkIO bankThread
    putStrLn ">>> Starting bank"
    threadDelay (3^7)

    _ <- forkIO mintetteThread
    putStrLn ">>> Starting mintette"
    threadDelay (5^7)
    
    _ <- forkIO userThread
    putStrLn ">>> Starting user"
    threadDelay (5^7)
