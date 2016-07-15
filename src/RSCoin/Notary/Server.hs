{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for Notary.

module RSCoin.Notary.Server
        ( serve
        , handlePublishTx
        , handlePollTxs
        , handleAnnounceNewPeriods
        , handleGetPeriodId
        , handleGetSignatures
        , handleQueryCompleteMS
        , handleRemoveCompleteMS
        , handleAllocateMultisig
        ) where

import           Control.Exception       (throwIO)
import           Control.Monad.Catch     (catch)
import           Control.Monad.IO.Class  (MonadIO, liftIO)

import           Data.Acid               (createCheckpoint)
import           Data.Acid.Advanced      (query', update')
import           Data.Text               (Text)

import           Formatting              (build, int, sformat, shown, (%))

import qualified RSCoin.Core             as C
import qualified RSCoin.Core.Protocol    as P
import           RSCoin.Notary.AcidState (AcquireSignatures (..),
                                          AddSignedTransaction (..),
                                          AllocateMSAddress (..),
                                          AnnounceNewPeriods (..),
                                          GetPeriodId (..), GetSignatures (..),
                                          PollTransactions (..),
                                          QueryAllMSAdresses (..),
                                          QueryCompleteMSAdresses (..),
                                          RSCoinNotaryState,
                                          RemoveCompleteMSAddresses (..))
import           RSCoin.Notary.Error     (NotaryError)
import           RSCoin.Timed            (ServerT, WorkMode,
                                          serverTypeRestriction0,
                                          serverTypeRestriction1,
                                          serverTypeRestriction2,
                                          serverTypeRestriction3,
                                          serverTypeRestriction4)

logError, logDebug :: MonadIO m => Text -> m ()
logError = C.logError C.notaryLoggerName
--logWarning = C.logWarning C.notaryLoggerName
--logInfo = C.logInfo C.notaryLoggerName
logDebug = C.logDebug C.notaryLoggerName

toServer1 a a1  = toServer0 $ a a1
toServer2 a a1 a2  = toServer0 $ a a1 a2
toServer3 a a1 a2 a3  = toServer0 $ a a1 a2 a3
toServer4 a a1 a2 a3 a4  = toServer0 $ a a1 a2 a3 a4

toServer0 :: WorkMode m => IO a -> ServerT m a
toServer0 action = liftIO $ action `catch` handler
  where
    handler (e :: NotaryError) = do
        logError $ sformat build e
        throwIO e

-- | Run Notary server which will process incoming sing requests.
serve
    :: WorkMode m
    => Int
    -> RSCoinNotaryState
    -> m ()
serve port notaryState = do
    idr1 <- (.toServer3) <$> serverTypeRestriction3
    idr2 <- (.toServer1) <$> serverTypeRestriction1
    idr3 <- (.toServer2) <$> serverTypeRestriction2
    idr4 <- (.toServer2) <$> serverTypeRestriction2
    idr5 <- (.toServer0) <$> serverTypeRestriction0
    idr6 <- (.toServer0) <$> serverTypeRestriction0
    idr7 <- (.toServer1) <$> serverTypeRestriction1
    idr8 <- (.toServer4) <$> serverTypeRestriction4
    P.serve
        port
        [ P.method (P.RSCNotary P.PublishTransaction)         $ idr1
            $ handlePublishTx notaryState
        , P.method (P.RSCNotary P.PollTransactions)           $ idr2
            $ handlePollTxs notaryState
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

handlePollTxs
    :: MonadIO m
    => RSCoinNotaryState
    -> [C.Address]
    -> m [(C.Address, [(C.Transaction, [(C.Address, C.Signature)])])]
handlePollTxs st addrs = do
    res <- query' st $ PollTransactions addrs
    --logDebug $ format' "Receiving polling request by addresses {}: {}" (addrs, res)
    return res

handlePublishTx
    :: MonadIO m
    => RSCoinNotaryState
    -> C.Transaction
    -> C.Address
    -> (C.Address, C.Signature)
    -> m [(C.Address, C.Signature)]
handlePublishTx st tx addr sg = do
    update' st $ AddSignedTransaction tx addr sg
    liftIO $ createCheckpoint st
    res <- update' st $ AcquireSignatures tx addr
    logDebug $ sformat ("Getting signatures for tx " % build % ", addr " % build % ": " % build)
        tx
        addr
        res
    return res

handleAnnounceNewPeriods
    :: MonadIO m
    => RSCoinNotaryState
    -> C.PeriodId
    -> [C.HBlock]
    -> m ()
handleAnnounceNewPeriods st pId hblocks = do
    update' st $ AnnounceNewPeriods pId hblocks
    logDebug $ sformat ("New period announcement, hblocks " % build % " from periodId " % int)
        hblocks
        pId

handleGetPeriodId :: MonadIO m => RSCoinNotaryState -> m C.PeriodId
handleGetPeriodId st = do
    res <- query' st GetPeriodId
    logDebug $ sformat ("Getting periodId: " % int) res
    return res

handleGetSignatures
    :: MonadIO m
    => RSCoinNotaryState
    -> C.Transaction
    -> C.Address
    -> m [(C.Address, C.Signature)]
handleGetSignatures st tx addr = do
    res <- query' st $ GetSignatures tx addr
    logDebug $ sformat ("Getting signatures for tx " % build % ", addr " % build % ": " % build)
        tx
        addr
        res
    return res

handleQueryCompleteMS
    :: MonadIO m
    => RSCoinNotaryState
    -> m [(C.Address, C.TxStrategy)]
handleQueryCompleteMS st = do
    res <- query' st QueryCompleteMSAdresses
    logDebug $ sformat ("Getting complete MS: " % shown) res
    return res

handleRemoveCompleteMS
    :: MonadIO m
    => RSCoinNotaryState
    -> [C.Address]
    -> m ()
handleRemoveCompleteMS st addresses = do
    logDebug $ sformat ("Removing complete MS of " % shown) addresses
    update' st $ RemoveCompleteMSAddresses addresses

handleAllocateMultisig
    :: MonadIO m
    => RSCoinNotaryState
    -> C.Address
    -> C.AllocationStrategy
    -> (C.Address, C.Signature)
    -> [(C.Signature, C.PublicKey)]
    -> m ()
handleAllocateMultisig st sAddr allocStrat sigPair chain = do
    logDebug "Begining allocation MS address..."
    update' st $ AllocateMSAddress sAddr allocStrat sigPair chain

    currentMSAddresses <- query' st QueryAllMSAdresses
    logDebug $ sformat ("All addresses: " % shown) currentMSAddresses
