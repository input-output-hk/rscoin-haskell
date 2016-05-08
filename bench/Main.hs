module Main where

import           Control.Monad.Trans (liftIO)
import           Data.Acid           (update)
import           Data.Text           as T

import qualified RSCoin.Bank         as B
import           RSCoin.Core         (Mintette (Mintette), Severity (Debug),
                                      constructPublicKey, defaultSecretKeyPath,
                                      initLogging, logWarning, readPublicKey,
                                      readSecretKey)
import qualified RSCoin.Mintette     as M

import           RSCoin.Test         (MsgPackRpc, bracket', runRealMode)

import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Exception   (bracket_)

import           Control.Monad.Extra (whenM)

import           System.Directory    (createDirectoryIfMissing,
                                      doesDirectoryExist,
                                      removeDirectoryRecursive)
import           System.FilePath     ((</>), takeDirectory)

homeKeyDirectory :: IO FilePath
homeKeyDirectory = takeDirectory <$> defaultSecretKeyPath

tempBenchDirectory :: FilePath
tempBenchDirectory = ".bench-local"

bankLocalDirectory :: FilePath
bankLocalDirectory = tempBenchDirectory </> "bank-db"

setupBench :: IO ()
setupBench = do
    createDirectoryIfMissing False tempBenchDirectory
    
    createDirectoryIfMissing False =<< homeKeyDirectory
    skPath <- defaultSecretKeyPath
    writeFile skPath "SecKey \"448d85e1261c2ce919bdbdf1b3830653e91380f4f22ef6d5b0edfb6537dd0772\""

cleanupBench :: IO ()
cleanupBench = do
    removeDirectoryRecursiveIfExists tempBenchDirectory
    removeDirectoryRecursiveIfExists =<< homeKeyDirectory
  where
    removeDirectoryRecursiveIfExists :: FilePath -> IO ()
    removeDirectoryRecursiveIfExists directoryName = 
        whenM (doesDirectoryExist directoryName) (removeDirectoryRecursive directoryName)

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

mintetteThread :: IO ()
mintetteThread = do
    initLogging Debug
    skPath <- defaultSecretKeyPath
    sk     <- readSecretKey skPath
    runRealMode $ M.serve 1234 (tempBenchDirectory </> "mintette-db") sk

bankThread :: IO ()
bankThread = bankBracket $ \st -> do
    skPath <- liftIO $ defaultSecretKeyPath
    sk     <- liftIO $ readSecretKey skPath
    B.runWorker sk st
    B.serve st

main :: IO ()
main = bracket_ setupBench cleanupBench $ do
    initLogging Debug
    addMintette
    forkIO mintetteThread
    forkIO bankThread
    threadDelay (10^7)
