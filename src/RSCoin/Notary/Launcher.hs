-- | Launch Notary stuff.

module RSCoin.Notary.Launcher
       ( launchNotaryReal
       ) where

import           Control.Lens                         ((&), (.~))
import           Control.Monad.Catch                  (bracket)
import           Control.Monad.Trans                  (MonadIO, liftIO)

import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import           RSCoin.Core                          (Severity (..))
import           RSCoin.Core.NodeConfig               (Host, NodeContext (_notaryAddr),
                                                       bankHost, defaultNodeContext)
import           RSCoin.Notary.AcidState              (RSCoinNotaryState,
                                                       closeState, openMemState,
                                                       openState)
import           RSCoin.Notary.Server                 (serve)
import           RSCoin.Notary.Web.Servant            (servantApp)
import           RSCoin.Timed                         (fork_, runRealMode)

launchNotaryReal :: Severity -> Maybe FilePath -> Int -> Host -> IO ()
launchNotaryReal logSeverity dbPath webPort newBankHost = do
    let open      = maybe openMemState openState dbPath
        nodeCtx   = defaultNodeContext & bankHost .~ newBankHost
        servePort = snd $ _notaryAddr nodeCtx
    runRealMode nodeCtx $
        bracket (liftIO open) (liftIO . closeState) $
        \st ->
             do fork_ $ serve servePort st
                launchWeb webPort logSeverity st

loggingMiddleware :: Severity -> Middleware
loggingMiddleware Debug = logStdoutDev
loggingMiddleware Info = logStdout
loggingMiddleware _ = id

launchWeb
    :: MonadIO m
    => Int -> Severity -> RSCoinNotaryState -> m ()
launchWeb port sev st =
    liftIO . run port . loggingMiddleware sev . servantApp $ st
