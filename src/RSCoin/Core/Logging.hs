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
       , undefinedLoggerName
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
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           System.Console.ANSI       (Color (Blue, Green, Red, Yellow),
                                            ColorIntensity (Vivid),
                                            ConsoleLayer (Foreground),
                                            SGR (Reset, SetColor), setSGRCode)
import           System.IO                 (stderr, stdout)
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger         (Priority (DEBUG, ERROR, INFO, WARNING),
                                            logM, removeHandler, rootLoggerName,
                                            setHandlers, setLevel,
                                            updateGlobalLogger)
import           RSCoin.Core.NamedLogging  (WithNamedLogger (..))
import           RSCoin.Core.Primitives    (LoggerName)

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
    stderrFormatter = simpleLogFormatter
        ("[$time] " ++ colorizer ERROR "[$loggername:$prio]: " ++ "$msg")
    stdoutFormatter h r@(pr, _) n =
        simpleLogFormatter (colorizer pr "[$loggername:$prio] " ++ "$msg") h r n

table :: Priority -> (String, String)
table priority = case priority of
    ERROR   -> (setColor Red   , reset)
    DEBUG   -> (setColor Green , reset)
    WARNING -> (setColor Yellow, reset)
    INFO    -> (setColor Blue  , reset)
    _       -> ("", "")
  where
    setColor color = setSGRCode [SetColor Foreground Vivid color]
    reset = setSGRCode [Reset]

colorizer :: Priority -> String -> String
colorizer pr s = before ++ s ++ after
  where
    (before, after) = table pr

bankLoggerName,
    communicationLoggerName,
    explorerLoggerName,
    mintetteLoggerName,
    notaryLoggerName,
    testingLoggerName,
    timedLoggerName,
    undefinedLoggerName,
    userLoggerName :: LoggerName
bankLoggerName          = "bank"
communicationLoggerName = "communication"
explorerLoggerName      = "explorer"
mintetteLoggerName      = "mintette"
notaryLoggerName        = "notary"
testingLoggerName       = "testing"
timedLoggerName         = "timed"
undefinedLoggerName     = "naked"
userLoggerName          = "user"

predefinedLoggers :: [LoggerName]
predefinedLoggers =
    [ bankLoggerName
    , communicationLoggerName
    , explorerLoggerName
    , mintetteLoggerName
    , notaryLoggerName
    , timedLoggerName
    , undefinedLoggerName
    , userLoggerName
    ]

logDebug :: (WithNamedLogger m, MonadIO m)
         => T.Text -> m ()
logDebug = logMessage Debug

logInfo :: (WithNamedLogger m, MonadIO m)
        => T.Text -> m ()
logInfo = logMessage Info

logWarning :: (WithNamedLogger m, MonadIO m)
        => T.Text -> m ()
logWarning = logMessage Warning

logError :: (WithNamedLogger m, MonadIO m)
         => T.Text -> m ()
logError = logMessage Error

logMessage
    :: (WithNamedLogger m, MonadIO m)
    => Severity -> T.Text -> m ()
logMessage severity t = do
    loggerName <- getLoggerFromContext
    liftIO . logM loggerName (convertSeverity severity) $ T.unpack t
