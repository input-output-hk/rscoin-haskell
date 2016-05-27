{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Data.Maybe                           (fromMaybe)
import           Formatting                           (sformat, shown, (%))
import           System.Clock                         (Clock (..), TimeSpec,
                                                       diffTimeSpec, getTime)

-- workaround to make stylish-haskell work :(
import           Options.Generic

import           System.IO.Temp                       (withSystemTempDirectory)

import           RSCoin.Core                          (PublicKey, SecretKey,
                                                       Severity (..),
                                                       initLogging)

import           BenchSingleUser.RSCoin.FilePathUtils (tempBenchDirectory)
import           BenchSingleUser.RSCoin.Logging       (initBenchLogger, logInfo)
import           BenchSingleUser.RSCoin.UserLogic     (runBankUser, userThread)

data BenchOptions = BenchOptions
    { severity      :: Maybe Severity <?> "severity for global logger"
    , benchSeverity :: Maybe Severity <?> "severity for bench logger"
    } deriving (Generic, Show)

instance ParseField  Severity
instance ParseFields Severity
instance ParseRecord Severity
instance ParseRecord BenchOptions

type KeyPairList = [(SecretKey, PublicKey)]

initializeSuperUser :: FilePath -> IO (TimeSpec, TimeSpec)
initializeSuperUser benchDir = do
    logInfo "Initializaing user in bankMode…"
    cpuTimeBefore <- getTime ProcessCPUTime
    wallTimeBefore <- getTime Realtime
    userThread benchDir runBankUser 0
    wallTimeAfter <- getTime Realtime
    cpuTimeAfter <- getTime ProcessCPUTime
    return
        ( cpuTimeAfter `diffTimeSpec` cpuTimeBefore
        , wallTimeAfter `diffTimeSpec` wallTimeBefore)

main :: IO ()
main = do
    BenchOptions{..} <- getRecord "rscoin-bench-single-user"
    let globalSeverity  = fromMaybe Error $ unHelpful severity
        bSeverity       = fromMaybe Info   $ unHelpful benchSeverity
    withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
        initLogging globalSeverity
        initBenchLogger bSeverity
        (cpuTime, wallTime) <- initializeSuperUser benchDir
        logInfo $ sformat ("Elapsed CPU time: " % shown) cpuTime
        logInfo $ sformat ("Elapsed wall-clock time: " % shown) wallTime
