{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for Notary.

module RSCoin.Notary.Server
        ( serve
        ) where

import           Control.Exception       (throwIO)
import           Control.Monad.Catch     (catch)
import           Control.Monad.IO.Class  (MonadIO, liftIO)

import           Data.Acid               (createCheckpoint)
import           Data.Acid.Advanced      (query', update')
import           Data.Set                (Set)
import           Data.Text               (Text)

import           Formatting              (build, int, shown, sformat, (%))

import qualified RSCoin.Core             as C
import qualified RSCoin.Core.Protocol    as P
import           RSCoin.Notary.AcidState (AcquireSignatures (..),
                                          AddSignedTransaction (..),
                                          AnnounceNewPeriods (..),
                                          AllocateMSAddress (..),
                                          GetPeriodId (..), GetSignatures (..),
                                          PollTransactions (..),
                                          QueryAllMSAdresses (..),
                                          QueryCompleteMSAdresses (..),
                                          RemoveCompleteMSAddresses (..),
                                          RSCoinNotaryState)
import           RSCoin.Notary.Error     (NotaryError)
import           RSCoin.Timed            (ServerT, WorkMode,
                                          serverTypeRestriction0,
                                          serverTypeRestriction1,
                                          serverTypeRestriction2,
                                          serverTypeRestriction3,
                                          serverTypeRestriction5)

logError, logDebug :: MonadIO m => Text -> m ()
logError = C.logError C.notaryLoggerName
--logWarning = C.logWarning C.notaryLoggerName
--logInfo = C.logInfo C.notaryLoggerName
logDebug = C.logDebug C.notaryLoggerName


-- | Run Notary server which will process incoming sing requests.
serve
    :: WorkMode m
    => Int
    -> RSCoinNotaryState
    -> m ()
serve port notaryState = do
    idr1 <- serverTypeRestriction3
    idr2 <- serverTypeRestriction1
    idr3 <- serverTypeRestriction2
    idr4 <- serverTypeRestriction2
    idr5 <- serverTypeRestriction0
    idr6 <- serverTypeRestriction0
    idr7 <- serverTypeRestriction1
    idr8 <- serverTypeRestriction5
    P.serve
        port
        [ P.method (P.RSCNotary P.PublishTransaction)         $ idr1
            $ handlePublishTx notaryState
        , P.method (P.RSCNotary P.PollTransactions)           $ idr2
            $ pollTxs notaryState
        , P.method (P.RSCNotary P.GetSignatures)              $ idr3
            $ handleGetSignatures notaryState
        , P.method (P.RSCNotary P.AnnounceNewPeriodsToNotary) $ idr4
            $ handleAnnounceNewPeriods notaryState
        , P.method (P.RSCNotary P.GetNotaryPeriod)            $ idr5
            $ handleGetPeriodId notaryState
        , P.method (P.RSCNotary P.QueryCompleteMS)            $ idr6
            $ handleQueryCompleteMS notaryState
        , P.method (P.RSCNotary P.RemoveCompleteMS)           $ idr7
            $ handleRemoveCompleteMS notaryState
        , P.method (P.RSCNotary P.AllocateMultisig)           $ idr8
            $ handleAllocateMultisig notaryState
        ]

toServer :: WorkMode m => IO a -> ServerT m a
toServer action = liftIO $ action `catch` handler
  where
    handler (e :: NotaryError) = do
        logError $ sformat build e
        throwIO e

pollTxs
    :: WorkMode m
    => RSCoinNotaryState
    -> [C.Address]
    -> ServerT m [(C.Address, [(C.Transaction, [(C.Address, C.Signature)])])]
pollTxs st addrs = toServer $ do
    res <- query' st $ PollTransactions addrs
    --logDebug $ format' "Receiving polling request by addresses {}: {}" (addrs, res)
    return res

handlePublishTx
    :: WorkMode m
    => RSCoinNotaryState
    -> C.Transaction
    -> C.Address
    -> (C.Address, C.Signature)
    -> ServerT m [(C.Address, C.Signature)]
handlePublishTx st tx addr sg = toServer $ do
    update' st $ AddSignedTransaction tx addr sg
    liftIO $ createCheckpoint st
    res <- update' st $ AcquireSignatures tx addr
    logDebug $ sformat ("Getting signatures for tx " % build % ", addr " % build % ": " % build)
        tx
        addr
        res
    return res

handleAnnounceNewPeriods
    :: WorkMode m
    => RSCoinNotaryState
    -> C.PeriodId
    -> [C.HBlock]
    -> ServerT m ()
handleAnnounceNewPeriods st pId hblocks = toServer $ do
    update' st $ AnnounceNewPeriods pId hblocks
    logDebug $ sformat ("New period announcement, hblocks " % build % " from periodId " % int)
        hblocks
        pId

handleGetPeriodId
    :: WorkMode m
    => RSCoinNotaryState
    -> ServerT m C.PeriodId
handleGetPeriodId st = toServer $ do
    res <- query' st GetPeriodId
    logDebug $ sformat ("Getting periodId: " % int) res
    return res

handleGetSignatures
    :: WorkMode m
    => RSCoinNotaryState
    -> C.Transaction
    -> C.Address
    -> ServerT m [(C.Address, C.Signature)]
handleGetSignatures st tx addr = toServer $ do
    res <- query' st $ GetSignatures tx addr
    logDebug $ sformat ("Getting signatures for tx " % build % ", addr " % build % ": " % build)
        tx
        addr
        res
    return res

handleQueryCompleteMS
    :: WorkMode m
    => RSCoinNotaryState
    -> ServerT m [(C.Address, C.TxStrategy)]
handleQueryCompleteMS st = toServer $ do
    res <- query' st QueryCompleteMSAdresses
    logDebug $ sformat ("Getting complete MS: " % shown) res
    return res

handleRemoveCompleteMS
    :: WorkMode m
    => RSCoinNotaryState
    -> [C.Address]
    -> ServerT m ()
handleRemoveCompleteMS st addresses = toServer $ do
    logDebug $ sformat ("Removing complete MS of " % shown) addresses
    update' st $ RemoveCompleteMSAddresses addresses

handleAllocateMultisig
    :: WorkMode m
    => RSCoinNotaryState
    -> C.Address
    -> Set C.Address
    -> Int
    -> (C.Address, C.Signature)
    -> [(C.Signature, C.PublicKey)]
    -> ServerT m ()
handleAllocateMultisig st sAddr parties m sigPair chain = toServer $ do
    logDebug "Begining allocation MS address..."
    update' st $ AllocateMSAddress sAddr parties m sigPair chain

    currentMSAddresses <- query' st QueryAllMSAdresses
    logDebug $ sformat ("All addresses: " % shown) currentMSAddresses
