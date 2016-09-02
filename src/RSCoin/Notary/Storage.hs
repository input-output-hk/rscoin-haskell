{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Storage for Notary's data.

module RSCoin.Notary.Storage
       ( Storage (_allocationEndurance, _masterKeys, _transactionEndurance)
       , addSignedTransaction
       , allocateMSAddress
       , announceNewPeriods
       , emptyNotaryStorage
       , getPeriodId
       , getSignatures
       , pollPendingTxs
       , queryAllMSAdresses
       , queryCompleteMSAdresses
       , queryMyMSRequests
       , removeCompleteMSAddresses
       , outdatedAllocs
       ) where

import           Control.Lens           (Getter, Lens', at, makeLenses, to, use,
                                         uses, view, views, (%=), (%~), (&),
                                         (.=), (?=), (^.))
import           Control.Monad          (forM_, unless, when)
import           Control.Monad.Catch    (MonadThrow (throwM))
import           Control.Monad.Extra    (unlessM, whenJust, whenM)

import           Data.Acid              (Query, Update, liftQuery)
import qualified Data.Foldable          as F
import           Data.Hashable          (Hashable)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM hiding (HashMap)
import           Data.HashSet           (HashSet)
import qualified Data.HashSet           as HS hiding (HashSet)
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IM hiding (IntMap)
import qualified Data.Map.Strict        as M hiding (Map)
import           Data.Maybe             (fromJust, fromMaybe, mapMaybe)
import           Formatting             (build, sformat, (%))

import           RSCoin.Core            (Address (..), HBlock (..), PeriodId,
                                         PublicKey, Signature, Transaction (..),
                                         Utxo, computeOutputAddrids,
                                         validateSignature, validateTxPure, verify)
import           RSCoin.Core.Strategy   (AddressToTxStrategyMap,
                                         AllocationAddress, AllocationInfo (..),
                                         AllocationStrategy (..), MSAddress,
                                         PartyAddress (..), TxStrategy (..),
                                         allParties, allocateTxFromAlloc,
                                         allocationStrategy,
                                         currentConfirmations,
                                         partyToAllocation)
import           RSCoin.Notary.Defaults (allocationAttemptsLimit,
                                         defaultAllocationEndurance,
                                         defaultTransactionEndurance)
import           RSCoin.Notary.Error    (NotaryError (..))

type TxPoolSignatureBundle = HashMap Address (Signature Transaction)

data Storage = Storage
    { -- | Pool of trasactions to be signed. Maps transaction to already
      -- collected signatures.
      _txPool                 :: !(HashMap Transaction TxPoolSignatureBundle)

      -- | Mapping between address and a set of unspent addrids, owned by it.
      -- Basically it is just Utxo^(-1).
      -- @TODO: replace 'Set' with 'HashSet'
      -- @TODO: do we need it?
      -- , _unspentAddresses       :: !(HashMap Address (Set AddrId))

      -- | Mapping from newly allocated multisignature addresses. This Map is
      -- used only during multisignature address allocation process.
    , _allocationStrategyPool :: !(HashMap MSAddress AllocationInfo)

      -- | Mapping PeriodId -> MSAddresses that were allocated during period.
    , _discardMSAddresses     :: !(IntMap [MSAddress])

      -- | Mapping PeriodId -> Transactions that were submitted during period.
    , _discardTransactions    :: !(IntMap [Transaction])

      -- | Constant that defines how many periods we should store MS address allocation
      -- requests (i.e. information in '_discardMSAddresses').
    , _allocationEndurance    :: !PeriodId

      -- | Constant that defines how many periods we should store transactions
      -- requests (i.e. information in '_discardTransactions').
    , _transactionEndurance   :: !PeriodId

      -- | Number of attempts for user per period to allocate multisig address.
    , _periodStats            :: !(HashMap Address Int)

      -- | Non-default addresses, registered in system (published to bank).
    , _addresses              :: !AddressToTxStrategyMap

      -- | Mapping between addrid and address.
    , _utxo                   :: !Utxo

      -- | Trusted master keys to check for signatures in MS address allocation &
      -- transaction signing.
    , _masterKeys             :: ![PublicKey]  -- @TODO: replace with HashSet

      -- | Last periodId, known to Notary.
    , _periodId               :: !PeriodId
    } deriving (Show)

$(makeLenses ''Storage)

emptyNotaryStorage :: Storage
emptyNotaryStorage =
    Storage
    { _txPool                 = HM.empty
    , _allocationStrategyPool = HM.empty
    , _discardMSAddresses     = IM.empty
    , _discardTransactions    = IM.empty
    , _allocationEndurance    = defaultAllocationEndurance
    , _transactionEndurance   = defaultTransactionEndurance
    , _periodStats            = HM.empty
    , _addresses              = M.empty
    , _utxo                   = M.empty
    , _masterKeys             = []
    , _periodId               = -1
    }

-- ==============
-- UPDATE SECTION
-- ==============

-----------------
-- ms alloccation
-----------------

-- | Throws NEBlocked if user reaches limit of attempts (DoS protection).
guardMaxAttemps :: Address -> Update Storage ()
guardMaxAttemps userAddr = do
    periodStats %=
        HM.insertWith (\new old -> min (old + new) allocationAttemptsLimit) userAddr 1
    currentAttemtps <- uses periodStats $ fromJust . HM.lookup userAddr
    when (currentAttemtps >= allocationAttemptsLimit) $ throwM NEBlocked

type MSSignature      = Signature (MSAddress, AllocationStrategy)
type MaybePKSignature = Maybe (PublicKey, Signature PublicKey)

-- | Allocate new multisignature address by chosen strategy and
-- given chain of certificates.
allocateMSAddress
    :: MSAddress                     -- ^ New multisig address itself
    -> PartyAddress                  -- ^ Address of party who call this
    -> AllocationStrategy            -- ^ Strategy for MS address allocation
    -> MSSignature                   -- ^ 'Signature' of @(msAddr, argStrategy)@
    -> MaybePKSignature              -- ^ Party address authorization.
                                     -- 1. cold master public key
                                     -- 2. signature of party by master key
    -> Update Storage ()
allocateMSAddress
    msAddr
    argPartyAddress
    argStrategy@AllocationStrategy{..}
    requestSig
    mMasterSlavePair
  = do
      -- too many checks :( I wish I know which one we shouldn't check
      -- but order of checks matters!!!
      let partyAddr@(Address partyPk) = partyAddress argPartyAddress
      let signedData = (msAddr, argStrategy)
      let slavePk    = case argPartyAddress of
              TrustParty{..} -> hotTrustKey
              UserParty{..}  -> partyPk

      trustedKeys <- use masterKeys
      unless (null trustedKeys) $ case mMasterSlavePair of
          Nothing -> throwM $ NEInvalidArguments "You should provide master pk and slave signature"
          Just (masterPk, masterSlaveSig) -> do
              unless (verify masterPk masterSlaveSig slavePk) $
                  throwM $ NEUnrelatedSignature "partyAddr not signed with masterPk"
              when (masterPk `notElem` trustedKeys) $
                  throwM $ NEInvalidArguments "provided master pk is not a trusted key"
      unless (verify slavePk requestSig signedData) $
          throwM $ NEUnrelatedSignature $ sformat
              ("(msAddr, strategy) not signed with proper sk for pk: " % build) slavePk
      when (HS.size _allParties < 2) $
          throwM $ NEInvalidStrategy "multisignature address should have at least two members"
      when (_sigNumber <= 0) $
          throwM $ NEInvalidStrategy "number of signatures to sign tx should be positive"
      when (_sigNumber > HS.size _allParties) $
          throwM $ NEInvalidStrategy
              "number of signatures to sign tx is greater then number of members"
      unless (partyToAllocation argPartyAddress `HS.member` _allParties) $
          throwM $ NEInvalidArguments "party address not in set of addresses"
      whenM (uses addresses $ M.member msAddr) $
        throwM $ NEInvalidArguments $ sformat
            ("ms address " % build % " already registered; please, regenerate new") msAddr

      guardMaxAttemps partyAddr

      mMSAddressInfo <- uses allocationStrategyPool $ HM.lookup msAddr
      let allocAddress = partyToAllocation argPartyAddress

      case mMSAddressInfo of
          Nothing -> do
              pId <- use periodId
              discardMSAddresses %= IM.alter (Just . (msAddr :) . fromMaybe []) pId
              allocationStrategyPool.at msAddr ?=
                  AllocationInfo { _allocationStrategy   = argStrategy
                                 , _currentConfirmations = HM.singleton allocAddress partyAddr }
          Just ainfo -> do
              when (ainfo^.allocationStrategy /= argStrategy) $
                  throwM $ NEInvalidArguments "result strategy for MS address is not equal to yours"

              allocationStrategyPool.at msAddr ?=
                  (ainfo & currentConfirmations %~ HM.insert allocAddress partyAddr)

-- | Remove all addresses from list (bank only usage).
removeCompleteMSAddresses :: PublicKey -> [MSAddress] -> Signature [MSAddress] -> Update Storage ()
removeCompleteMSAddresses bankPublicKey completeAddrs signedAddrs = do
    unless (verify bankPublicKey signedAddrs completeAddrs) $
        throwM $
        NEUnrelatedSignature "addr list in remove MS query not signed by bank"
    forM_ completeAddrs $
        \adress ->
             allocationStrategyPool %= HM.delete adress

---------------
-- transactions
---------------

-- | Receives @tx@, @msAddr@, @(partyAddr, sig)@ pair, checks
-- validity and publishes @(tx, partyAddr)@ to storage, adds @(partyAddr, sig)@
-- to list of already collected for particular @tx@ pair.
addSignedTransaction :: Transaction
                     -> MSAddress
                     -> (Address, Signature Transaction)
                     -> Update Storage ()
addSignedTransaction tx@Transaction{..} msAddr (partyAddr, sig) = do
    checkTransactionValidity
    checkAddrIdsKnown
--    checkAddrRelativeToTx
    checkSigRelativeToAddr

    pId <- use periodId
    discardTransactions %= IM.alter (Just . (tx :) . fromMaybe []) pId
    txPool %= HM.alter (Just . HM.insert partyAddr sig . fromMaybe HM.empty) tx
  where
    checkTransactionValidity =
        unless (validateTxPure tx) $
            throwM $ NEInvalidArguments $ sformat
                ("Transaction doesn't pass valitidy check: " % build)
                tx
    -- | Throws error if addrid isn't known (with corresponding label).
    -- User should repeat transaction after some timeout
    checkAddrIdsKnown = do
        curUtxo <- use utxo
        unless (all (`M.member` curUtxo) txInputs) $
            use periodId >>= throwM . NEAddrIdNotInUtxo
--    checkAddrRelativeToTx = do
--        s <- HM.lookupDefault S.empty addr <$> use unspentAddresses
--        unless (any (`S.member` s) txInputs) $
--            throwM NEAddrNotRelativeToTx
    checkSigRelativeToAddr = do
        strategy <- liftQuery (getStrategy msAddr)
        case strategy of
            DefaultStrategy ->
                throwM $ NEStrategyNotSupported "DefaultStrategy"
            MOfNStrategy _ addrs ->
                when (partyAddr `notElem` addrs) $
                    throwM $ NEUnrelatedSignature "party address not a member of this MS address"

        unless (validateSignature sig partyAddr tx) $
            throwM NEInvalidSignature

--------------------
-- handle period end
--------------------

-- | Clear all outdated information from `discard*` Maps.
removeOutdatedInfo
    :: (Eq info, Hashable info)
    => PeriodId
    -> Getter Storage PeriodId
    -> Lens' Storage (IntMap [info])
    -> Lens' Storage (HashMap info value)
    -> Update Storage ()
removeOutdatedInfo pId enduranceLens discardLens poolLens = do
    aliveInterval <- use enduranceLens

    unlessM (uses discardLens IM.null) $ do
        (oldestSavedPid, _) <- uses discardLens IM.findMin
        let deleteLookup = const $ const Nothing

        forM_ [oldestSavedPid .. pId - aliveInterval] $ \oldPid -> do
            (mInfoList, newDiscard) <- uses discardLens
                $ IM.updateLookupWithKey deleteLookup oldPid
            discardLens .= newDiscard -- @TODO: optimize and set only once

            whenJust mInfoList $ \infoList -> forM_ infoList $ \info ->
                poolLens %= HM.delete info


-- | Announce HBlocks, not yet known to Notary.
announceNewPeriods :: PeriodId -- ^ periodId of latest hblock
                   -> [HBlock] -- ^ blocks, head corresponds to the latest block
                   -> Update Storage ()
announceNewPeriods pId' blocks = do
    pId <- use periodId
    mapM_ announceNewPeriod $ reverse $ take (pId' - pId) blocks
    periodId .= pId'

    -- discard old MS address allocation requests
    removeOutdatedInfo
        pId
        allocationEndurance
        discardMSAddresses
        allocationStrategyPool

    -- discard old pending transactions
    removeOutdatedInfo
        pId
        transactionEndurance
        discardTransactions
        txPool

announceNewPeriod :: HBlock -> Update Storage ()
announceNewPeriod HBlock{..} = do
    addresses   %= M.union hbAddresses
    periodStats .= HM.empty
    forM_ hbTransactions processPublishedTransacion
  where
    processPublishedTransacion tx@Transaction{..} = do
        txPool %= HM.delete tx

        let txOuts = computeOutputAddrids tx
        forM_ txInputs $ \addrId ->
            utxo %= M.delete addrId
        forM_ txOuts   $ \(addrId, addr) ->
            utxo %= M.insert addrId addr

-- =============
-- QUERY SECTION
-- =============

-----------------
-- ms alloccation
-----------------

outdatedAllocs :: Query Storage (IntMap [MSAddress])
outdatedAllocs = view discardMSAddresses

queryMSAddressesHelper
    :: Lens' Storage (HashMap MSAddress info)
    -> (info -> Bool)
    -> (info -> meta)
    -> Query Storage [(MSAddress, meta)]
queryMSAddressesHelper poolLens selector mapper =
    view
    $ poolLens
    . to (HM.filter selector)
    . to (HM.map mapper)
    . to HM.toList

-- | Query all Multisignature addresses.
queryAllMSAdresses :: Query Storage [(MSAddress, AllocationInfo)]
queryAllMSAdresses = queryMSAddressesHelper allocationStrategyPool (const True) id

-- | Query all completed multisignature addresses
queryCompleteMSAdresses :: Query Storage [(MSAddress, TxStrategy)]
queryCompleteMSAdresses = queryMSAddressesHelper
    allocationStrategyPool
    (\ainfo ->
          ainfo^.allocationStrategy.allParties.to HS.size ==
          ainfo^.currentConfirmations.to HM.size)
    (allocateTxFromAlloc . _allocationStrategy)

-- | Request all address which contains 'allocAddress' as party.
queryMyMSRequests :: AllocationAddress -> Query Storage [(MSAddress, AllocationInfo)]
queryMyMSRequests allocAddress = queryMSAddressesHelper
    allocationStrategyPool
    (\ainfo -> ainfo^.allocationStrategy.allParties.to (HS.member allocAddress))
    id

---------------
-- transactions
---------------

-- | Get address strategy.
getStrategy :: Address -> Query Storage TxStrategy
getStrategy addr = fromMaybe DefaultStrategy . M.lookup addr <$> view addresses

-- | By given @tx@ get list of collected signatures (or empty list if (tx, addr)
-- is not registered/already removed from Notary).
getSignatures :: Transaction -> Query Storage [(Address, Signature Transaction)]
getSignatures tx = HM.toList . (HM.lookupDefault HM.empty tx) <$> view txPool

-- | Get last known periodId of Notary (interface for bank).
getPeriodId :: Query Storage PeriodId
getPeriodId = view periodId

-- | Collect all pending multisignature transactions which have one of
-- party is a member of given list.
-- @TODO: replace [Address] with @HashSet Address@ for faster checks
pollPendingTxs :: [Address] -> Query Storage [Transaction]
pollPendingTxs parties = do
    pendingTxs     <- views txPool HM.keys
    curUtxo        <- view utxo
    let resultTxSet = F.foldl' (partyFold curUtxo) HS.empty pendingTxs
    return $ HS.toList resultTxSet
  where
    partyFold :: Utxo
              -> HashSet Transaction
              -> Transaction
              -> HashSet Transaction
    partyFold addrIdResolve txSet tx@Transaction{..} =
        if any (`elem` parties)
           $ mapMaybe (`M.lookup` addrIdResolve) txInputs
        then HS.insert tx txSet
        else txSet
