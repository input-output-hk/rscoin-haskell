-- | Logigng within bench.

module Bench.RSCoin.Logging
       ( benchLoggerName
       , initBenchLogger
       , logDebug
       , logInfo
       , logWarning
       , logError
       ) where

import           Control.Monad.Trans (MonadIO)
import qualified Data.Text           as T

import qualified RSCoin.Core.Logging as L

benchLoggerName :: L.LoggerName
benchLoggerName = "bench"

initBenchLogger :: L.Severity -> IO ()
initBenchLogger = flip L.initLoggerByName benchLoggerName

logDebug :: MonadIO m
         => T.Text -> m ()
logDebug = L.logDebug benchLoggerName

logInfo :: MonadIO m
         => T.Text -> m ()
logInfo = L.logInfo benchLoggerName

logWarning :: MonadIO m
         => T.Text -> m ()
logWarning = L.logWarning benchLoggerName

logError :: MonadIO m
         => T.Text -> m ()
logError = L.logError benchLoggerName
