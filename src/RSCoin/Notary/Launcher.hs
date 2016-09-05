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

import           RSCoin.Core                          (ContextArgument (..),
                                                       PeriodId, PublicKey,
                                                       Severity (..),
                                                       notaryLoggerName,
                                                       runRealModeUntrusted)
import           RSCoin.Util.Timed                    (fork_)

import           RSCoin.Notary.AcidState              (NotaryState, closeState,
                                                       openMemState, openState)
import           RSCoin.Notary.Server                 (serveNotary)
import           RSCoin.Notary.Web.Servant            (servantApp)

launchNotaryReal :: Severity
                 -> Bool
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
            fork_ $ serveNotary st
            launchWeb webPort logSeverity st

loggingMiddleware :: Severity -> Middleware
loggingMiddleware Debug = logStdoutDev
loggingMiddleware Info  = logStdout
loggingMiddleware _     = id

launchWeb
    :: MonadIO m
    => Int -> Severity -> NotaryState -> m ()
launchWeb port sev st =
    liftIO . run port . loggingMiddleware sev . servantApp $ st
