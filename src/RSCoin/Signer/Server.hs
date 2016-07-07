{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for Signer.

module RSCoin.Signer.Server
        ( serve
        ) where

import           Control.Exception       (Exception (..), throwIO)
import           Control.Monad.Catch     (catch)
import           Control.Monad.IO.Class  (MonadIO, liftIO)

import           Data.Acid               (createCheckpoint)
import           Data.Acid.Advanced      (query', update')
import           Data.Text               (Text)
import           Data.Typeable           (Typeable)

import           Formatting              (sformat, shown, (%))

import qualified RSCoin.Core             as C
import qualified RSCoin.Core.Protocol    as P
import           RSCoin.Signer.AcidState (AcquireSignatures (..),
                                          AddSignature (..),
                                          AnnounceNewPeriods (..),
                                          GetPeriodId (..), GetSignatures (..),
                                          PollTransactions (..),
                                          RSCoinSignerState)
import           RSCoin.Signer.Error
import           RSCoin.Timed            (ServerT, WorkMode,
                                          serverTypeRestriction0,
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
    idr4 <- serverTypeRestriction2
    idr5 <- serverTypeRestriction0
    P.serve
        port
        [ P.method (P.RSCSign P.PublishTransaction) $ idr1 $ handlePublishTx signerState
        , P.method (P.RSCSign P.PollTransactions) $ idr2 $ pollTxs signerState
        , P.method (P.RSCSign P.GetSignatures) $ idr3 $ handleGetSignatures signerState
        , P.method (P.RSCSign P.AnnounceNewPeriodsToSigner) $ idr4 $ handleAnnounceNewPeriods signerState
        , P.method (P.RSCSign P.GetSignerPeriod) $ idr5 $ handleGetPeriodId signerState
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
pollTxs st addrs = toServer $ do
    res <- query' st $ PollTransactions addrs
    --logDebug $ format' "Receiving polling request by addresses {}: {}" (addrs, res)
    return res

handlePublishTx :: WorkMode m
    => RSCoinSignerState -> C.Transaction -> C.Address -> (C.Address, C.Signature) -> ServerT m [(C.Address, C.Signature)]
handlePublishTx st tx addr sg =
    toServer $
    do update' st $ AddSignature tx addr sg
       liftIO $ createCheckpoint st
       res <- update' st $ AcquireSignatures tx addr
       logDebug $
            format' "Getting signatures for tx {}, addr {}: {}" (tx, addr, res)
       return res

handleAnnounceNewPeriods :: WorkMode m
    => RSCoinSignerState -> C.PeriodId -> [C.HBlock] -> ServerT m ()
handleAnnounceNewPeriods st pId hblocks =
    toServer $
    do update' st $ AnnounceNewPeriods pId hblocks
       logDebug $
            format' "New period announcement, hblocks {} from periodId {}" (hblocks, pId)

handleGetPeriodId :: WorkMode m
    => RSCoinSignerState -> ServerT m C.PeriodId
handleGetPeriodId st =
    toServer $
    do res <- query' st $ GetPeriodId
       logDebug $
            formatSingle' "Getting periodId: {}" res
       return res

handleGetSignatures :: WorkMode m
    => RSCoinSignerState -> C.Transaction -> C.Address -> ServerT m [(C.Address, C.Signature)]
handleGetSignatures st tx addr =
    toServer $
    do res <- query' st $ GetSignatures tx addr
       logDebug $
            format' "Getting signatures for tx {}, addr {}: {}" (tx, addr, res)
       return res
