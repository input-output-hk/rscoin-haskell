module Main where

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.InfraThreads  (addMintette, bankThread,
                                             mintetteThread)
import           Bench.RSCoin.UserLogic     (benchUserTransactions, initializeBank,
                                             initializeUser, userThread)

import           RSCoin.Core                (Severity (Debug), initLogging,
                                             keyGen)

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.Async   (forConcurrently)

import           System.IO.Temp             (withSystemTempDirectory)

main :: IO ()
main = withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
    initLogging Debug

    (sk1, pk1) <- keyGen
    addMintette 1 benchDir pk1

    _ <- forkIO $ mintetteThread 1 benchDir sk1
    threadDelay (3 * 10^6)

    _ <- forkIO $ bankThread benchDir
    threadDelay (3 * 10^6)

    let initUserAction = userThread benchDir initializeUser
    let userIds = [1..2]
    userAddresses <- mapM initUserAction userIds

    -- give money to all users
    let bankId = 0
    userThread benchDir (initializeBank userAddresses) bankId
    threadDelay (20 * 10^6)

    putStrLn $ "Should be addr: " ++ show (userAddresses !! 0)
    let benchUserAction = userThread benchDir $ benchUserTransactions userAddresses
    _ <- forConcurrently userIds benchUserAction
    threadDelay (5 * 10^6)