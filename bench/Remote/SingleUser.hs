{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Exception          (finally)
import           Control.Monad              (when)
import           Data.Maybe                 (fromMaybe)
import           Data.Optional              (Optional (Specific), empty,
                                             optional)
import           Formatting                 (build, sformat, (%))

-- workaround to make stylish-haskell work :(
import           Options.Generic
import           System.IO.Temp             (withSystemTempDirectory)

import           Serokell.Util.Bench        (ElapsedTime, measureTime_)

import           RSCoin.Core                (Severity (..), initLogging)

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.UserCommons   (userThreadWithPath)
import           Bench.RSCoin.UserSingle    (printDynamicTPS,
                                             runSingleSuperUser, runSingleUser)

data BenchOptions = BenchOptions
    { severity      :: Maybe Severity <?> "severity for global logger"
    , benchSeverity :: Maybe Severity <?> "severity for bench logger"
    , transactions  :: Maybe Word     <?> "number of transactions"
    , walletDb      :: Maybe FilePath <?> "path to wallet (assuming it has enough money)"
    , dumpStats     :: FilePath       <?> "file name to dump statistics"
    , logInterval   :: Maybe Word     <?> "print number of executed transactions with this interval"
    , printDynamic  :: Bool           <?> "if `true` then will print how TPS changes over time"
    } deriving (Generic, Show)

instance ParseField  Word
instance ParseField  Severity
instance ParseFields Severity
instance ParseRecord Severity
instance ParseRecord BenchOptions

run
    :: Maybe Word
    -> Word
    -> FilePath
    -> Optional FilePath
    -> FilePath
    -> IO ElapsedTime
run interval txNum benchDir optWalletPath dumpFile =
    measureTime_ $
    userThreadWithPath
        benchDir
        (const $ doRun interval txNum dumpFile)
        0
        optWalletPath
  where
    doRun = optional runSingleSuperUser (const runSingleUser) optWalletPath

main :: IO ()
main = do
    BenchOptions{..}  <- getRecord "rscoin-bench-single-user"

    let globalSeverity = fromMaybe Error $ unHelpful severity
    let bSeverity      = fromMaybe Info  $ unHelpful benchSeverity
    let txNum          = fromMaybe 100   $ unHelpful transactions
    let walletPath     = maybe empty Specific $ unHelpful walletDb
    let dumpFile       = unHelpful dumpStats
    let interval       = unHelpful logInterval
    let shouldPrintTPS = unHelpful printDynamic

    withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
        initLogging globalSeverity
        initBenchLogger bSeverity

        elapsedTime <- run interval txNum benchDir walletPath dumpFile
                       `finally` when shouldPrintTPS (printDynamicTPS dumpFile)
        logInfo $ sformat ("Elapsed time: " % build) elapsedTime
