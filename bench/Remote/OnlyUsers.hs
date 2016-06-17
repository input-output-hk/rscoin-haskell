{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

import           Control.Concurrent.Async   (forConcurrently)
import           Data.ByteString            (ByteString)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text.IO               as TIO
import           Formatting                 (build, fixed, int, sformat, stext,
                                             (%))
import           System.IO.Temp             (withSystemTempDirectory)

-- workaround to make stylish-haskell work :(
import           Options.Generic

import           Serokell.Util.Text         (listBuilderCSV, show')

import           RSCoin.Core                (Address, Severity (..),
                                             initLogging)
import           RSCoin.User                (toAddress)

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.UserLogic     (benchUserTransactions,
                                             initializeBank, initializeUser,
                                             userThread)
import           Bench.RSCoin.Util          (ElapsedTime (elapsedWallTime),
                                             measureTime_, perSecond)

data BenchOptions = BenchOptions
    { users         :: Int            <?> "number of users"
    , transactions  :: Maybe Word     <?> "number of transactions per user"
    , severity      :: Maybe Severity <?> "severity for global logger"
    , benchSeverity :: Maybe Severity <?> "severity for bench logger"
    , bank          :: ByteString     <?> "bank host"
    , output        :: Maybe FilePath <?> "optional path to dump statistics"
    , mintettes     :: Maybe Word     <?> "number of mintettes (only for statistics)"
    , csv           :: Maybe FilePath <?> "optional path to dump statistics as csv"
    , csvPrefix     :: Maybe Text     <?> "extra values to prepend to csv"
    } deriving (Generic, Show)

instance ParseField  Word
instance ParseField  Severity
instance ParseFields Severity
instance ParseRecord Severity
instance ParseRecord BenchOptions

initializeUsers :: ByteString -> FilePath -> [Word] -> IO [Address]
initializeUsers bankHost benchDir userIds = do
    let initUserAction = userThread bankHost benchDir initializeUser
    logInfo $ sformat ("Initializing " % int % " users…") $ length userIds
    map toAddress <$> mapM initUserAction userIds

initializeSuperUser :: Word
                    -> ByteString
                    -> FilePath
                    -> [Address]
                    -> IO ()
initializeSuperUser txNum bankHost benchDir userAddresses = do
    let bankId = 0
    userThread
        bankHost
        benchDir
        (const $ initializeBank txNum userAddresses)
        bankId

runTransactions
    :: ByteString
    -> Word
    -> FilePath
    -> [Word]
    -> IO ElapsedTime
runTransactions bankHost transactionNum benchDir userIds = do
    let benchUserAction =
            userThread bankHost benchDir $ benchUserTransactions transactionNum
    logInfo "Running transactions…"
    measureTime_ $ forConcurrently userIds benchUserAction

data Statistics = Statistics
    { sUsersNum     :: !Word
    , sMintettesNum :: !(Maybe Word)
    , sTxNum        :: !Word
    , sTps          :: !Double
    , sElapsedTime  :: !ElapsedTime
    } deriving (Show)

mintettesNumStr :: Maybe Word -> Text
mintettesNumStr Nothing = "<unknown>"
mintettesNumStr (Just n) = show' n

dumpStatistics :: Statistics -> FilePath -> IO ()
dumpStatistics Statistics {..} fp =
    TIO.writeFile fp $
    sformat
        ("rscoin-bench-only-users statistics:\n" % "tps: " % fixed 2 %
         "\nusers: " %
         int %
         "\nmintettes: " %
         stext %
         "\ntransactions per user: " %
         int %
         "\ntransactions total: " %
         int %
         "\nelapsed time: " %
         build %
         "\n")
        sTps
        sUsersNum
        (mintettesNumStr sMintettesNum)
        sTxNum
        (sTxNum * sUsersNum)
        sElapsedTime

dumpCsv :: Statistics -> Text -> FilePath -> IO ()
dumpCsv Statistics{..} prefix fp =
    TIO.writeFile fp $
    sformat (stext % build) prefix $
    listBuilderCSV
        [ sformat (fixed 3) sTps
        , show' sUsersNum
        , mintettesNumStr sMintettesNum
        , show' sTxNum
        , show' (sTxNum * sUsersNum)]

main :: IO ()
main = do
    BenchOptions{..} <- getRecord "rscoin-bench-only-users"
    let userNumber = fromIntegral $ unHelpful users
        globalSeverity = fromMaybe Error $ unHelpful severity
        bSeverity = fromMaybe Info $ unHelpful benchSeverity
        transactionNum = fromMaybe 1000 $ unHelpful transactions
        bankHost = unHelpful bank
        csvPref = fromMaybe "" $ unHelpful csvPrefix
    withSystemTempDirectory tempBenchDirectory $
        \benchDir ->
             do initLogging globalSeverity
                initBenchLogger bSeverity
                let userIds = [1 .. userNumber]
                userAddresses <- initializeUsers bankHost benchDir userIds
                initializeSuperUser
                    transactionNum
                    bankHost
                    benchDir
                    userAddresses
                t <-
                    runTransactions
                        bankHost
                        transactionNum
                        benchDir
                        userIds
                logInfo . sformat ("Elapsed time: " % build) $ t
                let txTotal = transactionNum * userNumber
                    tps = perSecond txTotal $ elapsedWallTime t
                    statistics =
                        Statistics
                        { sUsersNum = userNumber
                        , sMintettesNum = unHelpful mintettes
                        , sTxNum = transactionNum
                        , sTps = tps
                        , sElapsedTime = t
                        }
                logInfo . sformat ("TPS: " % fixed 2) $ tps
                maybe (return ()) (dumpStatistics statistics) $ unHelpful output
                maybe (return ()) (dumpCsv statistics csvPref) $ unHelpful csv
