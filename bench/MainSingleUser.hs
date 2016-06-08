{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

import           Data.ByteString            (ByteString)
import           Data.Maybe                 (fromMaybe)
import           Formatting                 (sformat, shown, (%))
import           System.Clock               (Clock (..), TimeSpec, diffTimeSpec,
                                             getTime)

-- workaround to make stylish-haskell work :(
import           Options.Generic

import           System.IO.Temp             (withSystemTempDirectory)

import           RSCoin.Core                (PublicKey, SecretKey,
                                             Severity (..), initLogging)

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.UserLogic     (runBankUser, userThread)

data BenchOptions = BenchOptions
    { severity      :: Maybe Severity <?> "severity for global logger"
    , benchSeverity :: Maybe Severity <?> "severity for bench logger"
    , bank          :: ByteString     <?> "bank host"
    } deriving (Generic, Show)

instance ParseField  Severity
instance ParseFields Severity
instance ParseRecord Severity
instance ParseRecord BenchOptions

initializeSuperUser :: ByteString -> FilePath -> IO (TimeSpec, TimeSpec)
initializeSuperUser bankHost benchDir = do
    logInfo "Initializaing user in bankMode…"
    cpuTimeBefore <- getTime ProcessCPUTime
    wallTimeBefore <- getTime Realtime
    userThread bankHost benchDir (const runBankUser) 0
    wallTimeAfter <- getTime Realtime
    cpuTimeAfter <- getTime ProcessCPUTime
    return
        ( cpuTimeAfter `diffTimeSpec` cpuTimeBefore
        , wallTimeAfter `diffTimeSpec` wallTimeBefore)

main :: IO ()
main = do
    BenchOptions{..} <- getRecord "rscoin-bench-single-user"
    let globalSeverity  = fromMaybe Error $ unHelpful severity
        bSeverity       = fromMaybe Info $ unHelpful benchSeverity
        bankHost        = unHelpful bank
    withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
        initLogging globalSeverity
        initBenchLogger bSeverity
        (cpuTime, wallTime) <- initializeSuperUser bankHost benchDir
        logInfo $ sformat ("Elapsed CPU time: " % shown) cpuTime
        logInfo $ sformat ("Elapsed wall-clock time: " % shown) wallTime
