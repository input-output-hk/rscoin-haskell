{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for Notary.

module RSCoin.Notary.Server
        ( serveNotary
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
import           Control.Lens            ((^.))
import           Control.Monad.Catch     (catch)
import           Control.Monad.IO.Class  (MonadIO, liftIO)

import           Data.Acid               (createCheckpoint)
import           Data.Acid.Advanced      (query', update')

import           Formatting              (build, int, sformat, shown, (%))

import           Serokell.Util.Text      (pairBuilder)

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
                                          QueryMyMSRequests (..),
                                          RSCoinNotaryState,
                                          RemoveCompleteMSAddresses (..))
import           RSCoin.Notary.Error     (NotaryError)
import           RSCoin.Timed            (MonadRpc (getNodeContext),
                                          WorkMode, serverTypeRestriction0,
                                          serverTypeRestriction1,
                                          serverTypeRestriction2,
                                          serverTypeRestriction3,
                                          serverTypeRestriction5)

toServer :: MonadIO m => IO a -> m a
toServer action = liftIO $ action `catch` handler
  where
    handler (e :: NotaryError) = do
        C.logError $ sformat build e
        throwIO e

-- | Run Notary server which will process incoming sing requests.
serveNotary
    :: WorkMode m
    => RSCoinNotaryState
    -> m ()
serveNotary notaryState = do
    idr1 <- serverTypeRestriction3
    idr2 <- serverTypeRestriction1
    idr3 <- serverTypeRestriction2
    idr4 <- serverTypeRestriction2
    idr5 <- serverTypeRestriction0
    idr6 <- serverTypeRestriction0
    idr7 <- serverTypeRestriction2
    idr8 <- serverTypeRestriction5
    idr9 <- serverTypeRestriction1

    nodeCtx <- getNodeContext
    let bankPublicKey = nodeCtx ^. C.bankPublicKey
    let notaryPort    = nodeCtx ^. C.notaryPort

    P.serve
        notaryPort
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
            $ handleRemoveCompleteMS notaryState bankPublicKey
        , P.method (P.RSCNotary P.AllocateMultisig)           $ idr8
            $ handleAllocateMultisig notaryState
        , P.method (P.RSCNotary P.QueryMyAllocMS)             $ idr9
            $ handleQueryMyAllocationMS notaryState
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
handlePublishTx st tx addr sg = toServer $ do
    update' st $ AddSignedTransaction tx addr sg
    liftIO $ createCheckpoint st
    res <- update' st $ AcquireSignatures tx addr
    C.logDebug $ sformat ("Getting signatures for tx " % build % ", addr " % build % ": " % build)
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
handleAnnounceNewPeriods st pId hblocks = toServer $ do
    update' st $ AnnounceNewPeriods pId hblocks
    C.logDebug $ sformat ("New period announcement, hblocks " % build % " from periodId " % int)
        hblocks
        pId

handleGetPeriodId :: MonadIO m => RSCoinNotaryState -> m C.PeriodId
handleGetPeriodId st = toServer $ do
    res <- query' st GetPeriodId
    C.logDebug $ sformat ("Getting periodId: " % int) res
    return res

handleGetSignatures
    :: MonadIO m
    => RSCoinNotaryState
    -> C.Transaction
    -> C.Address
    -> m [(C.Address, C.Signature)]
handleGetSignatures st tx addr = toServer $ do
    res <- query' st $ GetSignatures tx addr
    C.logDebug $ sformat ("Getting signatures for tx " % build % ", addr " % build % ": " % build)
        tx
        addr
        res
    return res

handleQueryCompleteMS
    :: MonadIO m
    => RSCoinNotaryState
    -> m [(C.Address, C.TxStrategy)]
handleQueryCompleteMS st = toServer $ do
    res <- query' st QueryCompleteMSAdresses
    C.logDebug $ sformat ("Getting complete MS: " % shown) res
    return res

handleRemoveCompleteMS
    :: MonadIO m
    => RSCoinNotaryState
    -> C.PublicKey
    -> [C.Address]
    -> C.Signature
    -> m ()
handleRemoveCompleteMS st bankPublicKey addresses signedAddrs = toServer $ do
    C.logDebug $ sformat ("Removing complete MS of " % shown) addresses
    update' st $ RemoveCompleteMSAddresses bankPublicKey addresses signedAddrs

handleAllocateMultisig
    :: MonadIO m
    => RSCoinNotaryState
    -> C.Address
    -> C.PartyAddress
    -> C.AllocationStrategy
    -> C.Signature
    -> (C.PublicKey, C.Signature)
    -> m ()
handleAllocateMultisig st msAddr partyAddr allocStrat signature masterCheck = toServer $ do
    C.logDebug "Begining allocation MS address..."
    C.logDebug $
        sformat ("SigPair: " % build % ", Chain: " % build) signature (pairBuilder masterCheck)
    update' st $ AllocateMSAddress msAddr partyAddr allocStrat signature masterCheck

    -- @TODO: get query only in Debug mode
    currentMSAddresses <- query' st QueryAllMSAdresses
    C.logDebug $ sformat ("All addresses: " % shown) currentMSAddresses

handleQueryMyAllocationMS
    :: MonadIO m
    => RSCoinNotaryState
    -> C.AllocationAddress
    -> m [(C.MSAddress, C.AllocationInfo)]
handleQueryMyAllocationMS st allocAddr = toServer $ do
    C.logDebug "Querying my MS allocations..."
    query' st $ QueryMyMSRequests allocAddr
