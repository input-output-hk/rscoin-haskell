{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for Signer.

module RSCoin.Signer.Server
        ( serve
        ) where

import           Control.Exception       (Exception (..), throwIO)
import           Control.Monad.Catch     (catch)
import           Control.Monad.IO.Class  (MonadIO, liftIO)

import           Data.Acid.Advanced      (query', update')
import           Data.Text               (Text)
import           Data.Typeable           (Typeable)

import           Formatting              (sformat, shown, (%))

import qualified RSCoin.Core             as C
import qualified RSCoin.Core.Protocol    as P
import           RSCoin.Signer.AcidState (AddSignature (..), GetSignatures (..),
                                          RSCoinSignerState, AnnounceNewPeriod(..), PollTransactions(..))
import RSCoin.Signer.Error
import           RSCoin.Timed            (ServerT, WorkMode,
                                          serverTypeRestriction1,
                                          serverTypeRestriction2,
                                          serverTypeRestriction3)
import           Serokell.Util.Text      (format', formatSingle')

logError, logDebug :: MonadIO m => Text -> m ()
logError = C.logError C.signerLoggerName
--logWarning = C.logWarning C.mintetteLoggerName
--logInfo = C.logInfo C.signerLoggerName
logDebug = C.logDebug C.signerLoggerName


-- | Run Signer server which will process incoming sing requests.
serve
    :: WorkMode m
    => Int -> RSCoinSignerState -> m ()
serve port signerState = do
    idr1 <- serverTypeRestriction3
    idr2 <- serverTypeRestriction1
    idr3 <- serverTypeRestriction2
    idr4 <- serverTypeRestriction1
    P.serve
        port
        [ P.method (P.RSCSign P.PublishTransaction) $ idr1 $ handlePublishTx signerState
        , P.method (P.RSCSign P.PollTransactions) $ idr2 $ pollTxs signerState
        , P.method (P.RSCSign P.GetSignatures) $ idr3 $ handleGetSignatures signerState
        , P.method (P.RSCSign P.AnnounceNewPeriodToSigner) $ idr4 $ handleAnnounceNewPeriod signerState
        ]

toServer :: WorkMode m => IO a -> ServerT m a
toServer action = liftIO $ action `catch` handler
  where
    handler (e :: SignerError) = do
        logError $ sformat shown e
        throwIO e

pollTxs
    :: WorkMode m
    => RSCoinSignerState -> [C.Address] -> ServerT m [(C.Address, [(C.Transaction, [(C.Address, C.Signature)])])]
pollTxs _ addrs = toServer $ do
    res <- query' st $ PollTransactions addrs
    logDebug $ format' "Receiving polling request by addresses {}: {}" (addrs, res)
    return res

handlePublishTx :: WorkMode m
    => RSCoinSignerState -> C.Transaction -> C.Address -> (C.Address, C.Signature) -> ServerT m [(C.Address, C.Signature)]
handlePublishTx st tx addr sg =
    toServer $
    do update' st $ AddSignature tx addr sg
       res <- query' st $ GetSignatures tx addr
       logDebug $
            format' "Getting signatures for tx {}, addr {}: {}" (tx, addr, res)
       return res

handleAnnounceNewPeriod :: WorkMode m
    => RSCoinSignerState -> C.HBlock -> ServerT m ()
handleAnnounceNewPeriod st hblock =
    toServer $
    do res <- query' st $ AnnounceNewPeriod hblock
       logDebug $
            formatSingle' "New period announcement, hblock {}" hblock
       return res

handleGetSignatures :: WorkMode m
    => RSCoinSignerState -> C.Transaction -> C.Address -> ServerT m [(C.Address, C.Signature)]
handleGetSignatures st tx addr =
    toServer $
    do res <- query' st $ GetSignatures tx addr
       logDebug $
            format' "Getting signatures for tx {}, addr {}: {}" (tx, addr, res)
       return res
