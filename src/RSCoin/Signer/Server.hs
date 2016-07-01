{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for Signer.

module RSCoin.Signer.Server
        ( serve
        ) where

import           Control.Exception       (Exception (..), throwIO)
import           Control.Monad.Catch     (catch)
import           Control.Monad.IO.Class  (MonadIO, liftIO)

import           Data.Text               (Text)
import           Data.Typeable           (Typeable)

import           Formatting              (sformat, shown, (%))

import qualified RSCoin.Core             as C
import qualified RSCoin.Core.Protocol    as P
import           RSCoin.Signer.AcidState (RSCoinSignerState)
import           RSCoin.Timed            (ServerT, WorkMode,
                                          serverTypeRestriction1)

logError, logInfo :: MonadIO m => Text -> m ()
logError = C.logError C.signerLoggerName
--logWarning = C.logWarning C.mintetteLoggerName
logInfo = C.logInfo C.signerLoggerName
--logDebug = C.logDebug C.mintetteLoggerName

-- | Run Signer server which will process incoming sing requests.
serve
    :: WorkMode m
    => Int -> RSCoinSignerState -> m ()
serve port signerState = do
    idr1 <- serverTypeRestriction1
    P.serve
        port
        [ P.method (P.RSCSign P.SignTransaction) $
          idr1 $ signIncoming signerState]

-- TODO: move into better place
data SignerError = SignerError
        deriving (Show, Typeable)

instance Exception SignerError where
    toException = C.rscExceptionToException
    fromException = C.rscExceptionFromException

toServer :: WorkMode m => IO a -> ServerT m a
toServer action = liftIO $ action `catch` handler
  where
    handler (e :: SignerError) = do
        logError $ sformat shown e
        throwIO e

signIncoming
    :: WorkMode m
    => RSCoinSignerState -> C.Transaction -> ServerT m C.Transaction
signIncoming _ transaction = toServer $ do
    logInfo $ sformat ("Receiving transaction: " % shown) transaction
    return transaction
