{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import           Data.ByteString            (ByteString)
import           Data.Maybe                 (fromMaybe)
import           Formatting                 (build, sformat, (%))
-- workaround to make stylish-haskell work :(
import           Options.Generic
import           System.IO.Temp             (withSystemTempDirectory)

import           RSCoin.Core                (Severity (..), initLogging)

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.UserLogic     (runSingleUser, userThread)
import           Bench.RSCoin.Util          (ElapsedTime, measureTime_)

data BenchOptions = BenchOptions
    { severity      :: Maybe Severity <?> "severity for global logger"
    , benchSeverity :: Maybe Severity <?> "severity for bench logger"
    , bank          :: ByteString     <?> "bank host"
    , transactions  :: Maybe Word     <?> "number of transactions"
    } deriving (Generic, Show)

instance ParseField  Word
instance ParseField  Severity
instance ParseFields Severity
instance ParseRecord Severity
instance ParseRecord BenchOptions

run :: Word -> ByteString -> FilePath -> IO ElapsedTime
run txNum bankHost benchDir =
    measureTime_ $ userThread bankHost benchDir (const $ runSingleUser txNum) 0

main :: IO ()
main = do
    BenchOptions{..} <- getRecord "rscoin-bench-single-user"
    let globalSeverity = fromMaybe Error $ unHelpful severity
        bSeverity = fromMaybe Info $ unHelpful benchSeverity
        bankHost = unHelpful bank
        txNum = fromMaybe 100 $ unHelpful transactions
    withSystemTempDirectory tempBenchDirectory $
        \benchDir ->
             do initLogging globalSeverity
                initBenchLogger bSeverity
                logInfo . sformat ("Elapsed time: " % build) =<<
                    run txNum bankHost benchDir
