{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for Notary.

module RSCoin.Notary.Server
       ( serveNotary
       , handlePublishTx
       , handleAnnounceNewPeriod
       , handleGetPeriodId
       , handleGetPeriodIdUnsigned
       , handleGetSignatures
       , handleQueryCompleteMS
       , handleRemoveCompleteMS
       , handleAllocateMultisig
       ) where

import           Control.Applicative     (liftA2)
import           Control.Lens            (view, (^.))
import           Control.Monad           (unless, when)
import           Control.Monad.Catch     (catch, throwM)
import           Control.Monad.Trans     (lift)
import           Data.Binary             (Binary)
import           Data.Text               (Text)

import           Formatting              (build, int, sformat, shown, (%))

import           Serokell.Util.Text      (pairBuilder, show')

import           Control.TimeWarp.Rpc    (ServerT, serverTypeRestriction0,
                                          serverTypeRestriction1,
                                          serverTypeRestriction2,
                                          serverTypeRestriction3,
                                          serverTypeRestriction5)
import qualified RSCoin.Core             as C
import qualified RSCoin.Core.Protocol    as P

import           RSCoin.Notary.AcidState (AddSignedTransaction (..),
                                          AllocateMSAddress (..),
                                          AnnounceNewPeriod (..),
                                          GetPeriodId (..), GetSignatures (..),
                                          NotaryState, PollPendingTxs (..),
                                          QueryAllMSAdresses (..),
                                          QueryCompleteMSAdresses (..),
                                          QueryMyMSRequests (..),
                                          RemoveCompleteMSAddresses (..), query,
                                          tidyState, update)
import           RSCoin.Notary.Error     (NotaryError (..))

type ServerTE m a = ServerT m (Either Text a)

type ServerTESigned m a = ServerTE m (C.WithSignature a)

toServer
    :: C.WorkMode m
    => m a
    -> ServerTE m a
toServer action = lift $ (Right <$> action) `catch` handler
  where
    handler (e :: NotaryError) = do
        C.logError $ sformat build e
        return $ Left $ show' e

signHandler
    :: (Binary a, Functor m)
    => C.SecretKey -> ServerTE m a -> ServerTESigned m a
signHandler sk = fmap (fmap (C.mkWithSignature sk))

toServerSigned
    :: (Binary a, C.WorkMode m)
    => C.SecretKey
    -> m a
    -> ServerTESigned m a
toServerSigned sk = signHandler sk . toServer

-- | Run Notary server which will process incoming sing requests.
serveNotary
    :: C.WorkMode m
    => C.SecretKey -> NotaryState -> m ()
serveNotary sk st = do
    idr1 <- serverTypeRestriction3
    idr2 <- serverTypeRestriction2
    idr3 <- serverTypeRestriction1
    idr4 <- serverTypeRestriction0
    idr5 <- serverTypeRestriction0
    idr6 <- serverTypeRestriction2
    idr7 <- serverTypeRestriction5
    idr8 <- serverTypeRestriction1
    idr9 <- serverTypeRestriction1

    (bankPublicKey, notaryPort) <- liftA2 (,) (^. C.bankPublicKey) (^. C.notaryPort)
                                   <$> C.getNodeContext
    P.serve
        notaryPort
        [ P.method (P.RSCNotary P.PublishTransaction)         $ idr1
            $ handlePublishTx sk st
        , P.method (P.RSCNotary P.GetSignatures)              $ idr2
            $ handleGetSignatures sk st
        , P.method (P.RSCNotary P.AnnounceNewPeriodsToNotary) $ idr3
            $ handleAnnounceNewPeriod st
        , P.method (P.RSCNotary P.GetNotaryPeriod)            $ idr4
            $ handleGetPeriodId sk st
        , P.method (P.RSCNotary P.QueryCompleteMS)            $ idr5
            $ handleQueryCompleteMS sk st
        , P.method (P.RSCNotary P.RemoveCompleteMS)           $ idr6
            $ handleRemoveCompleteMS st bankPublicKey
        , P.method (P.RSCNotary P.AllocateMultisig)           $ idr7
            $ handleAllocateMultisig st
        , P.method (P.RSCNotary P.QueryMyAllocMS)             $ idr8
            $ handleQueryMyAllocationMS sk st
        , P.method (P.RSCNotary P.PollPendingTransactions)    $ idr9
            $ handlePollPendingTxs sk st
        ]

handlePublishTx
    :: C.WorkMode m
    => C.SecretKey
    -> NotaryState
    -> C.Transaction
    -> C.Address
    -> (C.Address, C.Signature C.Transaction)
    -> ServerTESigned m [(C.Address, C.Signature C.Transaction)]
handlePublishTx sk st tx addr sg =
    toServerSigned sk $
    do C.guardTransactionValidity tx
       update st $ AddSignedTransaction tx addr sg
       res <- query st $ GetSignatures tx
       C.logDebug $ sformat
           ("Getting signatures for tx " % build % ", addr " % build % ": " % build)
           tx
           addr
           res
       return res

handleAnnounceNewPeriod
    :: C.WorkMode m
    => NotaryState
    -> C.WithSignature (C.PeriodId, C.HBlock)
    -> ServerTE m ()
handleAnnounceNewPeriod st signed = toServer $ do
--    DEBUG
--    outdatedAllocs <- query st OutdatedAllocs
--    C.logDebug $ sformat ("All discard info: " % shown) outdatedAllocs
    bankPublicKey <- view C.bankPublicKey <$> C.getNodeContext
    unless (C.verifyWithSignature bankPublicKey signed) $
        throwM NEInvalidSignature

    let (pId, hblock) = C.wsValue signed
    C.logDebug $ sformat ("Bank sent pid: " % int) pId
    update st $ AnnounceNewPeriod pId hblock
    tidyState st
    C.logDebug $ sformat ("New period announcement, hblock " % build % " from periodId " % int)
        hblock
        pId

handleGetPeriodId
    :: C.WorkMode m
    => C.SecretKey -> NotaryState -> ServerTESigned m C.PeriodId
handleGetPeriodId sk st = signHandler sk $ handleGetPeriodIdUnsigned st

handleGetPeriodIdUnsigned
    :: C.WorkMode m
    => NotaryState -> ServerTE m C.PeriodId
handleGetPeriodIdUnsigned st =
    toServer $
    do res <- query st GetPeriodId
       res <$ C.logDebug (sformat ("Getting periodId: " % int) res)

handleGetSignatures
    :: C.WorkMode m
    => C.SecretKey
    -> NotaryState
    -> C.Transaction
    -> C.Address
    -> ServerTESigned m [(C.Address, C.Signature C.Transaction)]
handleGetSignatures sk st tx addr =
    toServerSigned sk $
    do C.guardTransactionValidity tx
       res <- query st $ GetSignatures tx
       C.logDebug $
           sformat
               ("Getting signatures for tx " % build % ", addr " % build % ": " %
                build)
               tx
               addr
               res
       return res

handleQueryCompleteMS
    :: C.WorkMode m
    => C.SecretKey
    -> NotaryState
    -> ServerTESigned m [(C.Address, C.TxStrategy)]
handleQueryCompleteMS sk st =
    toServerSigned sk $
    do res <- query st QueryCompleteMSAdresses
       C.logDebug $ sformat ("Getting complete MS: " % shown) res
       return res

handleRemoveCompleteMS
    :: C.WorkMode m
    => NotaryState
    -> C.PublicKey
    -> [C.Address]
    -> C.Signature [C.MSAddress]
    -> ServerTE m ()
handleRemoveCompleteMS st bankPublicKey addresses signedAddrs = toServer $ do
    C.logDebug $ sformat ("Removing complete MS of " % shown) addresses
    update st $ RemoveCompleteMSAddresses bankPublicKey addresses signedAddrs

handleAllocateMultisig
    :: C.WorkMode m
    => NotaryState
    -> C.Address
    -> C.PartyAddress
    -> C.AllocationStrategy
    -> C.Signature (C.MSAddress, C.AllocationStrategy)
    -> Maybe (C.PublicKey, C.Signature C.PublicKey)
    -> ServerTE m ()
handleAllocateMultisig st msAddr partyAddr allocStrat signature mMasterCheck = toServer $ do
    C.logDebug $ sformat ("Received Alloc|Confirm request from: " % build) partyAddr
    C.logDebug $ sformat ("Signature of (MS, Stragety): " % build % "\nChain: " % build)
        signature
        (pairBuilder <$> mMasterCheck)
    update st $ AllocateMSAddress msAddr partyAddr allocStrat signature mMasterCheck

    -- @TODO: get query only in Debug mode
    currentMSAddresses <- query st QueryAllMSAdresses
    C.logDebug $ sformat ("All addresses: " % shown) currentMSAddresses

handleQueryMyAllocationMS
    :: C.WorkMode m
    => C.SecretKey
    -> NotaryState
    -> C.AllocationAddress
    -> ServerTESigned m [(C.MSAddress, C.AllocationInfo)]
handleQueryMyAllocationMS sk st allocAddr =
    toServerSigned sk $
    do C.logDebug "Querying my MS allocations..."
       query st $ QueryMyMSRequests allocAddr

handlePollPendingTxs
    :: C.WorkMode m
    => C.SecretKey
    -> NotaryState
    -> [C.Address]
    -> ServerTESigned m [C.Transaction]
handlePollPendingTxs sk st parties =
    toServerSigned sk $
    do when (length parties > C.pollTransactionsLimit) $
           throwM $ C.BadRequest "too many addresses given"
       C.logDebug "Polling pending txs..."
       query st $ PollPendingTxs parties
