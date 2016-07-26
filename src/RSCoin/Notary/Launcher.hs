-- | Launch Notary stuff.

module RSCoin.Notary.Launcher
       ( launchNotaryReal
       ) where

import           Control.Monad.Catch                  (bracket)
import           Control.Monad.Trans                  (MonadIO, liftIO)
import           Data.ByteString                      (ByteString)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import           RSCoin.Core                          (NodeContext (..),
                                                       Severity (..),
                                                       defaultLayout')
import           RSCoin.Notary.AcidState              (RSCoinNotaryState,
                                                       closeState, openMemState,
                                                       openState)
import           RSCoin.Notary.Server                 (serve)
import           RSCoin.Notary.Web.Servant            (servantApp)
import           RSCoin.Timed                         (fork_, runRealMode)

launchNotaryReal :: Severity -> Maybe FilePath -> Int -> ByteString -> IO ()
launchNotaryReal logSeverity dbPath webPort bankHost = do
    let open = maybe openMemState openState dbPath
        layout = defaultLayout' bankHost
        servePort = snd $ _notaryAddr layout
    runRealMode layout $
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
