{-# LANGUAGE ViewPatterns #-}

-- | Logging functionality.

module RSCoin.Core.Logging
       ( Severity (..)
       , initLogging
       , logDebug
       , logInfo
       , logWarning
       , logError
       , logMessage
       ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as T
import           System.Log.Logger      (Priority (DEBUG, ERROR, INFO, WARNING),
                                         logM, rootLoggerName, setLevel,
                                         updateGlobalLogger)

-- | This type is intended to be used as command line option
-- which specifies which messages to print.
data Severity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Show, Read, Eq)

convertSeverity :: Severity -> Priority
convertSeverity Debug = DEBUG
convertSeverity Info = INFO
convertSeverity Warning = WARNING
convertSeverity Error = ERROR

initLogging :: Severity -> IO ()
initLogging (convertSeverity -> s) =
    updateGlobalLogger rootLoggerName $ setLevel s

logDebug :: MonadIO m
         => T.Text -> m ()
logDebug = logMessage Debug

logInfo :: MonadIO m
        => T.Text -> m ()
logInfo = logMessage Info

logWarning :: MonadIO m
        => T.Text -> m ()
logWarning = logMessage Warning

logError :: MonadIO m
        => T.Text -> m ()
logError = logMessage Error

logMessage
  :: MonadIO m
  => Severity -> T.Text -> m ()
logMessage severity =
    liftIO . logM rootLoggerName (convertSeverity severity) . T.unpack
