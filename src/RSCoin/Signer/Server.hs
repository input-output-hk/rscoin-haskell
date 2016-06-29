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

import           Formatting              (int, sformat, shown, (%))

import qualified RSCoin.Core             as C
import qualified RSCoin.Core.Protocol    as P
import           RSCoin.Signer.AcidState (RSCoinSignerState)
import           RSCoin.Timed            (ServerT, WorkMode,
                                          serverTypeRestriction0)

logError :: MonadIO m => Text -> m ()
logError = C.logError C.mintetteLoggerName
--logWarning = C.logWarning C.mintetteLoggerName
--logInfo = C.logInfo C.mintetteLoggerName
--logDebug = C.logDebug C.mintetteLoggerName

-- | Run Signer server which will process incoming sing requests.
serve
    :: WorkMode m
    => Int
    -> RSCoinSignerState
    -> m ()
serve port signerState = do
    idr1 <- serverTypeRestriction0
    P.serve port
        [ P.method (P.RSCSign P.SignTransaction) $ idr1 $ signIncoming signerState ]

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
    => RSCoinSignerState
    -> ServerT m ()
signIncoming st = toServer $ do undefined
    --mts <- query' st GetMintettes
    --logDebug bankLoggerName $ sformat ("Getting list of mintettes: " % int) mts
    --return mts