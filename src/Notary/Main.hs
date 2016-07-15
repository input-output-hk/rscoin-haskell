module Main where

import           Control.Monad.Catch                  (bracket)
import           Control.Monad.Trans                  (MonadIO, liftIO)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           RSCoin.Core                          (PlatformLayout (..),
                                                       Severity (..),
                                                       defaultLayout',
                                                       initLogging)
import qualified RSCoin.Notary                        as N
import           RSCoin.Timed                         (fork_, runRealMode)

import qualified NotaryOptions                        as Opts
import           RSCoin.Notary.Web.Servant            (servantApp)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cliLogSeverity
    let open = if cliMemMode
               then N.openMemState
               else N.openState cliPath
    let layout = defaultLayout' cliBankHost
        servePort = snd $ getNotaryAddr layout
    runRealMode layout $
        bracket (liftIO open) (liftIO . N.closeState) $
          \st -> do
              fork_ $ N.serve servePort st
              launchWeb cliWebPort cliLogSeverity st

loggingMiddleware :: Severity -> Middleware
loggingMiddleware Debug = logStdoutDev
loggingMiddleware Info = logStdout
loggingMiddleware _ = id

launchWeb :: MonadIO m => Int -> Severity -> N.RSCoinNotaryState -> m ()
launchWeb port sev st = liftIO . run port . loggingMiddleware sev . servantApp $ st
