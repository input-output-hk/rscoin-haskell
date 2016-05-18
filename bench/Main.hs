module Main where

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.InfraThreads  (addMintette, bankThread,
                                             defaultBenchPeriod, mintetteThread)
import           Bench.RSCoin.UserLogic     (benchUserTransactions,
                                             initializeBank, initializeUser,
                                             userThread)

import           Data.Int                   (Int64)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Data.Time.Units            (toMicroseconds)

import           RSCoin.Core                (PublicKey, SecretKey,
                                             Severity (..), bankLoggerName,
                                             initLoggerByName, initLogging,
                                             keyGen)
import           RSCoin.User.Wallet         (UserAddress)

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.Async   (forConcurrently)
import           Control.Monad              (forM_, replicateM, void)

import           System.IO.Temp             (withSystemTempDirectory)

benchMintetteNumber :: Int
benchMintetteNumber = 30

benchUserNumber :: Int64
benchUserNumber = 2

type KeyPairList = [(SecretKey, PublicKey)]

generateMintetteKeys :: Int -> IO KeyPairList
generateMintetteKeys n = replicateM n keyGen

runMintettes :: FilePath -> KeyPairList -> IO ()
runMintettes benchDir secretKeys
    = forM_ (zip [1..] secretKeys) $ \(mintetteId, (secretKey, publicKey)) -> do
        addMintette mintetteId benchDir publicKey
        void $ forkIO $ mintetteThread mintetteId benchDir secretKey

curTime :: IO Int
curTime = round <$> getPOSIXTime

establishMintettes :: FilePath -> IO ()
establishMintettes benchDir = do
    keyPairs <- generateMintetteKeys benchMintetteNumber
    runMintettes benchDir keyPairs
    putStrLn $ "Running " ++ show benchMintetteNumber ++ " mintettes..."
    threadDelay $ 5 * 10 ^ (6 :: Int)

establishBank :: FilePath -> IO ()
establishBank benchDir = do
    _ <- forkIO $ bankThread benchDir
    putStrLn "Running bank..."
    threadDelay $ 3 * 10 ^ (6 :: Int)

initializeUsers :: FilePath -> [Int64] -> IO [UserAddress]
initializeUsers benchDir userIds = do
    let initUserAction = userThread benchDir initializeUser
    putStrLn $ "Initializing " ++ show benchUserNumber ++ " users..."
    mapM initUserAction userIds

initializeSuperUser :: FilePath -> [UserAddress] -> IO ()
initializeSuperUser benchDir userAddresses = do
    -- give money to all users
    let bankId = 0
    userThread benchDir (initializeBank userAddresses) bankId
    putStrLn "Running user in bankMode and waiting for period end..."
    threadDelay $ fromInteger $ toMicroseconds (defaultBenchPeriod + 1)

runTransactions :: FilePath -> [UserAddress] -> [Int64] -> IO Int
runTransactions benchDir userAddresses userIds = do
    let benchUserAction = userThread benchDir $ benchUserTransactions userAddresses

    timeBefore <- curTime
    _ <- forConcurrently userIds benchUserAction
    timeAfter <- curTime

    return $ timeAfter - timeBefore

main :: IO ()
main = withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
    initLogging Warning
    initLoggerByName Info bankLoggerName

    establishMintettes benchDir
    establishBank      benchDir

    let userIds    = [1..benchUserNumber]
    userAddresses <- initializeUsers benchDir userIds
    initializeSuperUser benchDir userAddresses

    elapsedTime <- runTransactions benchDir userAddresses userIds
    putStrLn $ "Elapsed time: " ++ show elapsedTime
