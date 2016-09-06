-- | Convenience functions to launch explorer or do high-level operations
-- with it.

module RSCoin.Explorer.Launcher
       ( ContextArgument (..)
       , explorerWrapperReal
       , launchExplorerReal
       , launchExplorer
       , launchWeb
       ) where

import           Control.Monad.Catch                  (bracket)
import           Control.Monad.Trans                  (MonadIO (liftIO))

import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import           Control.TimeWarp.Timed               (fork_)
import           RSCoin.Core                          (ContextArgument (..),
                                                       RealMode, SecretKey,
                                                       Severity (..), WorkMode,
                                                       explorerLoggerName,
                                                       initLoggerByName,
                                                       runRealModeUntrusted)

import           RSCoin.Explorer.AcidState            (State, closeState,
                                                       openState)
import           RSCoin.Explorer.Channel              (Channel, newChannel)
import           RSCoin.Explorer.Server               (serve)
import qualified RSCoin.Explorer.Web                  as Web

explorerWrapperReal :: Bool
                    -> FilePath
                    -> ContextArgument
                    -> (State -> RealMode a)
                    -> IO a
explorerWrapperReal deleteIfExists storagePath ca =
    runRealModeUntrusted explorerLoggerName ca .
    bracket (openState deleteIfExists storagePath) closeState

launchExplorerReal :: Bool
                   -> Int
                   -> Int
                   -> Severity
                   -> FilePath
                   -> ContextArgument
                   -> SecretKey
                   -> IO ()
launchExplorerReal deleteIfExists portRpc portWeb severity storagePath ca sk = do
    channel <- newChannel
    explorerWrapperReal deleteIfExists storagePath ca $
        \st -> do
            fork_ $ launchExplorer portRpc sk channel st
            launchWeb portWeb severity channel st

launchExplorer
    :: WorkMode m
    => Int -> SecretKey -> Channel -> State -> m ()
launchExplorer port sk ch st = serve port ch st sk

loggingMiddleware :: Severity -> Middleware
loggingMiddleware Debug = logStdoutDev
loggingMiddleware Info  = logStdout
loggingMiddleware _     = id

launchWeb
    :: MonadIO m
    => Int -> Severity -> Channel -> State -> m ()
launchWeb port sev ch st = do
    app <- Web.mkApplication ch st
    liftIO $ initLoggerByName sev Web.wsLoggerName  -- @TODO: this logger name never used
    liftIO . run port . loggingMiddleware sev $ app
