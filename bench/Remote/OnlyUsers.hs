{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (forConcurrently)
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Maybe                 (fromMaybe)
import           Formatting                 (build, int, sformat, (%))
import           System.IO.Temp             (withSystemTempDirectory)

-- workaround to make stylish-haskell work :(
import           Options.Generic

import           RSCoin.Core                (Severity (..), bankSecretKey,
                                             defaultPeriodDelta, finishPeriod,
                                             initLogging)
import           RSCoin.Timed               (runRealMode)
import           RSCoin.User.Wallet         (UserAddress)

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.UserLogic     (benchUserTransactions,
                                             initializeBank, initializeUser,
                                             userThread)
import           Bench.RSCoin.Util          (ElapsedTime, measureTime_)

data BenchOptions = BenchOptions
    { users         :: Int            <?> "number of users"
    , transactions  :: Maybe Word     <?> "number of transactions per user"
    , severity      :: Maybe Severity <?> "severity for global logger"
    , benchSeverity :: Maybe Severity <?> "severity for bench logger"
    , bank          :: ByteString     <?> "bank host"
    } deriving (Generic, Show)

instance ParseField  Word
instance ParseField  Severity
instance ParseFields Severity
instance ParseRecord Severity
instance ParseRecord BenchOptions

initializeUsers :: ByteString -> FilePath -> [Int64] -> IO [UserAddress]
initializeUsers bankHost benchDir userIds = do
    let initUserAction = userThread bankHost benchDir initializeUser
    logInfo $ sformat ("Initializing " % int % " users…") $ length userIds
    mapM initUserAction userIds

initializeSuperUser :: Word
                    -> ByteString
                    -> FilePath
                    -> [UserAddress]
                    -> IO ()
initializeSuperUser txNum bankHost benchDir userAddresses = do
    -- give money to all users
    let bankId = 0
    logInfo "Initializaing user in bankMode…"
    userThread bankHost benchDir (const $ initializeBank txNum userAddresses) bankId
    logInfo
        "Initialized user in bankMode, finishing period"
    runRealMode bankHost $ finishPeriod bankSecretKey
    threadDelay $ 1 * 10 ^ (6 :: Int)

runTransactions
    :: ByteString
    -> Word
    -> FilePath
    -> [UserAddress]
    -> [Int64]
    -> IO ElapsedTime
runTransactions bankHost transactionNum benchDir userAddresses userIds = do
    let benchUserAction =
            userThread bankHost benchDir $
            benchUserTransactions transactionNum userAddresses
    logInfo "Running transactions…"
    measureTime_ $ forConcurrently userIds benchUserAction

main :: IO ()
main = do
    BenchOptions{..} <- getRecord "rscoin-bench-only-users"
    let userNumber      = unHelpful users
        globalSeverity  = fromMaybe Error $ unHelpful severity
        bSeverity       = fromMaybe Info $ unHelpful benchSeverity
        transactionNum  = fromMaybe 1000 $ unHelpful transactions
        bankHost        = unHelpful bank
    withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
        initLogging globalSeverity
        initBenchLogger bSeverity

        let userIds    = [1 .. fromIntegral userNumber]
        userAddresses <- initializeUsers bankHost benchDir userIds
        initializeSuperUser transactionNum bankHost benchDir userAddresses

        logInfo . sformat ("Elapsed time: " % build) =<<
            runTransactions bankHost transactionNum benchDir userAddresses userIds
