{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ViewPatterns       #-}

-- | Logging functionality.

module RSCoin.Core.Logging
       ( Severity (..)
       , initLogging
       , initLoggerByName
       , LoggerName
       , bankLoggerName
       , mintetteLoggerName
       , userLoggerName
       , timedLoggerName
       , communicationLoggerName
       , testingLoggerName
       , logDebug
       , logInfo
       , logWarning
       , logError
       , logMessage
       ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           System.IO                 (stderr, stdout)
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger         (Priority (DEBUG, ERROR, INFO, WARNING),
                                            logM, removeHandler, rootLoggerName,
                                            setHandlers, setLevel,
                                            updateGlobalLogger)

-- | This type is intended to be used as command line option
-- which specifies which messages to print.
data Severity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Generic, Typeable, Show, Read, Eq)

convertSeverity :: Severity -> Priority
convertSeverity Debug = DEBUG
convertSeverity Info = INFO
convertSeverity Warning = WARNING
convertSeverity Error = ERROR

initLogging :: Severity -> IO ()
initLogging sev = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    mapM_ (initLoggerByName sev) predefinedLoggers

initLoggerByName :: Severity -> LoggerName -> IO ()
initLoggerByName (convertSeverity -> s) name = do
    stdoutHandler <-
        (flip setFormatter) stdoutFormatter <$> streamHandler stdout s
    stderrHandler <-
        (flip setFormatter) stderrFormatter <$> streamHandler stderr ERROR
    updateGlobalLogger name $ setHandlers [stdoutHandler, stderrHandler]
  where
    stderrFormatter = simpleLogFormatter "[$time] [$loggername] $prio: $msg"
    stdoutFormatter h r@(pr,_) n
      | pr > DEBUG = simpleLogFormatter "[$loggername] $msg" h r n
    stdoutFormatter h r n
      | otherwise = simpleLogFormatter "[$loggername] $msg" h r n

type LoggerName = String

bankLoggerName, mintetteLoggerName, userLoggerName, timedLoggerName, communicationLoggerName, testingLoggerName :: LoggerName
bankLoggerName = "bank"
mintetteLoggerName = "mintette"
userLoggerName = "user"
timedLoggerName = "timed"
communicationLoggerName = "communication"
testingLoggerName = "testing"

predefinedLoggers :: [LoggerName]
predefinedLoggers =
    [ bankLoggerName
    , mintetteLoggerName
    , userLoggerName
    , timedLoggerName
    , communicationLoggerName]

logDebug :: MonadIO m
         => LoggerName -> T.Text -> m ()
logDebug = logMessage Debug

logInfo :: MonadIO m
        => LoggerName -> T.Text -> m ()
logInfo = logMessage Info

logWarning :: MonadIO m
        => LoggerName -> T.Text -> m ()
logWarning = logMessage Warning

logError :: MonadIO m
        => LoggerName -> T.Text -> m ()
logError = logMessage Error

logMessage
    :: MonadIO m
    => Severity -> LoggerName -> T.Text -> m ()
logMessage severity loggerName =
    liftIO . logM loggerName (convertSeverity severity) . T.unpack
