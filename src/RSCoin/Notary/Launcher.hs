-- | Launch Notary stuff.

module RSCoin.Notary.Launcher
       ( ContextArgument (..)
       , launchNotaryReal
       ) where

import           Control.Monad.Catch                  (bracket)
import           Control.Monad.Trans                  (MonadIO, liftIO)
import           Data.Optional                        (Optional)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import           Control.TimeWarp.Timed               (fork_)
import           RSCoin.Core                          (ContextArgument (..),
                                                       NodeContext, PeriodId,
                                                       PublicKey, SecretKey,
                                                       Severity (..),
                                                       getNodeContext,
                                                       notaryLoggerName,
                                                       runRealModeUntrusted)

import           RSCoin.Notary.AcidState              (NotaryState, closeState,
                                                       openMemState, openState)
import           RSCoin.Notary.Server                 (serveNotary)
import           RSCoin.Notary.Web.Servant            (servantApp)
import           RSCoin.Notary.Worker                 (runFetchWorker)

launchNotaryReal
    :: Severity
    -> Bool
    -> SecretKey
    -> Maybe FilePath
    -> ContextArgument
    -> Int
    -> [PublicKey]
    -> Optional PeriodId
    -> Optional PeriodId
    -> IO ()
launchNotaryReal
    logSeverity
    deleteIfExists
    sk
    dbPath
    ca
    webPort
    trustedKeys
    allocationEndurance
    transactionEndurance
  = do
    let openAction = maybe openMemState (openState deleteIfExists) dbPath
    runRealModeUntrusted notaryLoggerName ca $
        bracket (openAction trustedKeys allocationEndurance transactionEndurance) closeState $
        \st -> do
            fork_ $ serveNotary sk st
            fork_ $ runFetchWorker st
            launchWeb webPort logSeverity st =<< getNodeContext

loggingMiddleware :: Severity -> Middleware
loggingMiddleware Debug = logStdoutDev
loggingMiddleware Info  = logStdout
loggingMiddleware _     = id

launchWeb
    :: MonadIO m
    => Int
    -> Severity
    -> NotaryState
    -> NodeContext
    -> m ()
launchWeb port sev st nodeCtx =
    liftIO $ run port $ loggingMiddleware sev $ servantApp st nodeCtx
