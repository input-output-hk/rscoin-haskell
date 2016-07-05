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
                                          serverTypeRestriction1,
                                          serverTypeRestriction2,
                                          serverTypeRestriction3)

logError, logInfo :: MonadIO m => Text -> m ()
logError = C.logError C.signerLoggerName
--logWarning = C.logWarning C.mintetteLoggerName
logInfo = C.logInfo C.signerLoggerName
logDebug = C.logDebug C.mintetteLoggerName

-- | Run Signer server which will process incoming sing requests.
serve
    :: WorkMode m
    => Int -> RSCoinSignerState -> m ()
serve port signerState = do
    idr1 <- serverTypeRestriction3
    idr2 <- serverTypeRestriction1
    idr3 <- serverTypeRestriction2
    P.serve
        port
        [ P.method (P.RSCSign P.PublishTransaction) $ idr1 $ publishTx signerState
        , P.method (P.RSCSign P.PollTransactions) $ idr2 $ pollTxs signerState
        , P.method (P.RSCSign P.GetSignatures) $ idr3 $ getSignatures signerState
        ]

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

-- @TODO implement Signer's pollTxs and publishTx methods

pollTxs
    :: WorkMode m
    => RSCoinSignerState -> [C.Address] -> ServerT m [(C.Transaction, [(C.Address, C.Signature)])]
pollTxs _ addrs = toServer $ do
    logDebug $ sformat ("Receiving polling request by addresses " % shown) addrs
    return []

getSignatures
    :: WorkMode m
    => RSCoinSignerState -> C.Transaction -> C.Address -> ServerT m [(C.Address, C.Signature)]
getSignatures = undefined
publishTx
    :: WorkMode m
    => RSCoinSignerState -> C.Transaction -> C.Address -> (C.Address, C.Signature) -> ServerT m [(C.Address, C.Signature)]
publishTx _ tx addr sg = toServer $ do
    logInfo $ sformat ("Receiving address " % shown % " within transaction " % shown % " with signatures " % shown) addr tx sg
    return [sg]
