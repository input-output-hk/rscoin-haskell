{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (forConcurrently)
import           Data.Int                   (Int64)
import           Data.Time.Clock            (NominalDiffTime, diffUTCTime,
                                             getCurrentTime)
import           Data.Time.Units            (toMicroseconds)
import           Formatting                 (build, sformat, shown, (%))

-- workaround to make stylish-haskell work :(
import           Options.Generic

import           System.IO.Temp             (withSystemTempDirectory)

import           RSCoin.Core                (Severity (..), initLogging)
import           RSCoin.User.Wallet         (UserAddress)

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.InfraThreads  (defaultBenchPeriod)
import           Bench.RSCoin.UserLogic     (benchUserTransactions,
                                             initializeBank, initializeUser,
                                             userThread)

data UserBenchOptions = UserBenchOptions
    { users         :: Int            <?> "number of users"
    , severity      :: Maybe Severity <?> "severity for global logger"
    , benchSeverity :: Maybe Severity <?> "severity for bench logger"
    } deriving (Generic, Show)

instance ParseField  Severity
instance ParseFields Severity
instance ParseRecord Severity
instance ParseRecord UserBenchOptions

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
    UserBenchOptions{..} <- getRecord "rscoin-user-bench"
    let userNumber        = unHelpful users
    let globalSeverity    = maybe Error id $ unHelpful severity
    let bSeverity         = maybe Info  id $ unHelpful benchSeverity
    withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
        initLogging globalSeverity
        initBenchLogger bSeverity

        let userIds    = [1 .. fromIntegral userNumber]
        userAddresses <- initializeUsers benchDir userIds
        initializeSuperUser benchDir userAddresses

        elapsedTime <- runTransactions benchDir userAddresses userIds
        logInfo $ sformat ("Elapsed time: " % shown) elapsedTime
