{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.Async   (forConcurrently)
import           Control.Monad              (forM_, replicateM, void)
import           Data.Int                   (Int64)
import           Data.Time.Clock            (NominalDiffTime, diffUTCTime,
                                             getCurrentTime)
import           Data.Time.Units            (toMicroseconds)
import           Formatting                 (build, sformat, shown, (%))

-- workaround to make stylish-haskell work :(
import           Options.Generic

import           System.IO.Temp             (withSystemTempDirectory)

import           RSCoin.Core                (PublicKey, SecretKey,
                                             Severity (..), initLogging, keyGen)
import           RSCoin.User.Wallet         (UserAddress)

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.InfraThreads  (addMintette, bankThread,
                                             defaultBenchPeriod, mintetteThread)
import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.UserLogic     (benchUserTransactions,
                                             initializeBank, initializeUser,
                                             userThread)

data BenchOptions = BenchOptions
    { users         :: Int            <?> "number of users"
    , mintettes     :: Int            <?> "number of mintettes"
    , severity      :: Maybe Severity <?> "severity for global logger"
    , benchSeverity :: Maybe Severity <?> "severity for bench logger"
    } deriving (Generic, Show)

instance ParseField  Severity
instance ParseFields Severity
instance ParseRecord Severity
instance ParseRecord BenchOptions

type KeyPairList = [(SecretKey, PublicKey)]

generateMintetteKeys :: Int -> IO KeyPairList
generateMintetteKeys n = replicateM n keyGen

runMintettes :: FilePath -> KeyPairList -> IO ()
runMintettes benchDir secretKeys
    = forM_ (zip [1..] secretKeys) $ \(mintetteId, (secretKey, publicKey)) -> do
        addMintette mintetteId benchDir publicKey
        void $ forkIO $ mintetteThread mintetteId benchDir secretKey

establishMintettes :: FilePath -> Int -> IO ()
establishMintettes benchDir mintettesNumber = do
    keyPairs <- generateMintetteKeys mintettesNumber
    runMintettes benchDir keyPairs
    logInfo $ sformat ("Running " % build % " mintettes…") mintettesNumber
    threadDelay $ 5 * 10 ^ (6 :: Int)

establishBank :: FilePath -> IO ()
establishBank benchDir = do
    _ <- forkIO $ bankThread benchDir
    logInfo "Running bank..."
    threadDelay $ 3 * 10 ^ (6 :: Int)

initializeUsers :: FilePath -> [Int64] -> IO [UserAddress]
initializeUsers benchDir userIds = do
    let initUserAction = userThread benchDir initializeUser
    logInfo $ sformat ("Initializing " % build % " users…") $ length userIds
    mapM initUserAction userIds

initializeSuperUser :: FilePath -> [UserAddress] -> IO ()
initializeSuperUser benchDir userAddresses = do
    -- give money to all users
    let bankId = 0
    logInfo "Initializaing user in bankMode…"
    userThread benchDir (const $ initializeBank userAddresses) bankId
    logInfo "Initialized user in bankMode, now waiting for the end of the period…"
    threadDelay $ fromInteger $ toMicroseconds (defaultBenchPeriod + 1)

runTransactions :: FilePath -> [UserAddress] -> [Int64] -> IO NominalDiffTime
runTransactions benchDir userAddresses userIds = do
    let benchUserAction = userThread benchDir $ benchUserTransactions userAddresses

    logInfo "Running transactions…"
    timeBefore <- getCurrentTime
    _ <- forConcurrently userIds benchUserAction
    timeAfter <- getCurrentTime

    return $ timeAfter `diffUTCTime` timeBefore

main :: IO ()
main = do
    BenchOptions{..} <- getRecord "rscoin-user-bench"
    let mintettesNumber = unHelpful mintettes
    let userNumber      = unHelpful users
    let globalSeverity  = maybe Warning id $ unHelpful severity
    let bSeverity       = maybe Debug   id $ unHelpful benchSeverity
    withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
        initLogging globalSeverity
        initBenchLogger bSeverity

        establishMintettes benchDir mintettesNumber
        establishBank      benchDir

        let userIds    = [1 .. fromIntegral userNumber]
        userAddresses <- initializeUsers benchDir userIds
        initializeSuperUser benchDir userAddresses

        elapsedTime <- runTransactions benchDir userAddresses userIds
        logInfo $ sformat ("Elapsed time: " % shown) elapsedTime
