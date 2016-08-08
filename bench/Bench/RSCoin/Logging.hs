-- | Logging within bench.

module Bench.RSCoin.Logging
       ( benchLoggerName
       , initBenchLogger
       , logDebug
       , logInfo
       , logWarning
       , logError
       ) where

import           Control.Monad.Trans (MonadIO)
import qualified Data.Text                as T

import qualified RSCoin.Core.Logging      as L
import qualified RSCoin.Core.NamedLogging as L

benchLoggerName :: L.LoggerName
benchLoggerName = "bench"

initBenchLogger :: L.Severity -> IO ()
initBenchLogger = flip L.initLoggerByName "naked"

logDebug :: (MonadIO m, L.WithNamedLogger m)
         => T.Text -> m ()
logDebug = L.logDebug

logInfo :: (MonadIO m, L.WithNamedLogger m)
         => T.Text -> m ()
logInfo = L.logInfo

logWarning :: (MonadIO m, L.WithNamedLogger m)
         => T.Text -> m ()
logWarning = L.logWarning

logError :: (MonadIO m, L.WithNamedLogger m)
         => T.Text -> m ()
logError = L.logError
