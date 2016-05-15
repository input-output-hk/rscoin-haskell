module Main where

import           Bench.RSCoin.FilePathUtils (defaultBankKey, tempBenchDirectory)
import           Bench.RSCoin.InfraThreads  (addMintette, bankThread,
                                             mintetteThread)
import           Bench.RSCoin.UserLogic     (benchUserTransactions, initializeBank,
                                             initializeUser, userThread)

import           RSCoin.Core                (Severity (Debug), initLogging,
                                             keyGen)

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.Async   (async, forConcurrently, wait)

import           System.FilePath            ((</>))

import           System.IO.Temp             (withSystemTempDirectory)

import           Control.Monad              (forM_)

main :: IO ()
main = withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
    let bankKeyFilePath = benchDir </> "rscoin-key"
    writeFile bankKeyFilePath defaultBankKey

    initLogging Debug

    (sk1, pk1) <- keyGen
    addMintette 1 benchDir pk1

    _ <- forkIO $ mintetteThread 1 benchDir sk1
    threadDelay (3 * 10^6)

    _ <- forkIO $ bankThread benchDir bankKeyFilePath
    threadDelay (3 * 10^6)

    let initUserAction = userThread benchDir initializeUser
    let userIds = [1..2]
    userAddresses <- mapM initUserAction userIds

    -- give money to all users
    let bankId = 0
    userThread benchDir (initializeBank bankKeyFilePath userAddresses) bankId
    threadDelay (5 * 10^6)

    putStrLn $ "Should be addr: " ++ show (userAddresses !! 0)
    let benchUserAction = userThread benchDir $ benchUserTransactions userAddresses
    _ <- forM_ userIds benchUserAction
    threadDelay (5 * 10^6)