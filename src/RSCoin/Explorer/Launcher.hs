-- | Convenience functions to launch explorer or do high-level operations
-- with it.

module RSCoin.Explorer.Launcher
       ( explorerWrapperReal
       , launchExplorerReal
       , launchExplorer
       , launchWeb
       ) where

import           Control.Lens                         ((&), (.~))
import           Control.Monad.Catch                  (bracket)
import           Control.Monad.Trans                  (MonadIO (liftIO))

import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import           RSCoin.Core                          (SecretKey, Severity (..),
                                                       initLoggerByName)
import           RSCoin.Core.NodeConfig               (Host, bankHost, defaultNodeContext)
import           RSCoin.Timed                         (MsgPackRpc, WorkMode,
                                                       fork_, runRealMode)

import           RSCoin.Explorer.AcidState            (State, closeState,
                                                       openState)
import           RSCoin.Explorer.Channel              (Channel, newChannel)
import           RSCoin.Explorer.Server               (serve)
import qualified RSCoin.Explorer.Web                  as Web

explorerWrapperReal :: Host -> FilePath -> (State -> MsgPackRpc a) -> IO a
explorerWrapperReal newBankHost storagePath =
    runRealMode modifiedContext .
    bracket (liftIO $ openState storagePath) (liftIO . closeState)
  where
    modifiedContext = defaultNodeContext & bankHost .~ newBankHost

launchExplorerReal :: Host
                   -> Int
                   -> Int
                   -> Severity
                   -> FilePath
                   -> SecretKey
                   -> IO ()
launchExplorerReal newBankHost portRpc portWeb severity storagePath sk = do
    channel <- newChannel
    explorerWrapperReal newBankHost storagePath $
        \st ->
             do fork_ $ launchExplorer portRpc sk channel st
                launchWeb portWeb severity channel st

launchExplorer
    :: WorkMode m
    => Int -> SecretKey -> Channel -> State -> m ()
launchExplorer port sk ch st = serve port ch st sk

loggingMiddleware :: Severity -> Middleware
loggingMiddleware Debug = logStdoutDev
loggingMiddleware Info = logStdout
loggingMiddleware _ = id

launchWeb
    :: MonadIO m
    => Int -> Severity -> Channel -> State -> m ()
launchWeb port sev ch st = do
    app <- Web.mkApplication ch st
    liftIO $ initLoggerByName sev Web.wsLoggerName
    liftIO . run port . loggingMiddleware sev $ app
