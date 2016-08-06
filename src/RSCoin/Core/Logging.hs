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
       , notaryLoggerName
       , explorerLoggerName
       , timedLoggerName
       , userLoggerName
       , communicationLoggerName
       , testingLoggerName
       , logDebug
       , logInfo
       , logWarning
       , logError
       , logMessage
       ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Maybe                (fromJust)
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           System.Console.ANSI
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
    stderrFormatter = simpleLogFormatter ("[$time] " ++ colorizer ERROR "[$loggername:$prio]: " ++ "$msg")
    stdoutFormatter h r@(pr,_) n
      | pr > DEBUG = simpleLogFormatter (colorizer pr "[$loggername:$prio]" ++ "$msg") h r n
      | otherwise = simpleLogFormatter (colorizer pr "[$loggername:$prio]" ++ "$msg") h r n

table :: [(Priority, String)]
table = [ (ERROR, concatMap setSGRCode [[SetColor Background Vivid Red], [SetColor Foreground Vivid White]])
        , (DEBUG, concatMap setSGRCode [[SetColor Background Vivid Blue], [SetColor Foreground Vivid White]])
        , (WARNING, concatMap setSGRCode [[SetColor Background Vivid Yellow], [SetColor Foreground Vivid Black]])
        , (INFO, concatMap setSGRCode [[SetColor Background Vivid Green], [SetColor Foreground Vivid Black]])]

colorizer :: Priority -> String -> String
colorizer pr s = (fromJust $ lookup pr table) ++ s ++ setSGRCode [Reset]

type LoggerName = String

bankLoggerName,
    communicationLoggerName,
    explorerLoggerName,
    mintetteLoggerName,
    notaryLoggerName,
    testingLoggerName,
    timedLoggerName,
    userLoggerName :: LoggerName
bankLoggerName          = "bank"
communicationLoggerName = "communication"
explorerLoggerName      = "explorer"
mintetteLoggerName      = "mintette"
notaryLoggerName        = "notary"
testingLoggerName       = "testing"
timedLoggerName         = "timed"
userLoggerName          = "user"

predefinedLoggers :: [LoggerName]
predefinedLoggers =
    [ bankLoggerName
    , communicationLoggerName
    , explorerLoggerName
    , mintetteLoggerName
    , notaryLoggerName
    , timedLoggerName
    , userLoggerName
    ]

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
