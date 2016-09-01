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

import           Control.Lens           (Lens', at, makeLenses, to, use, uses,
                                         view, (%=), (%~), (&), (.=), (?=),
                                         (^.))
import           Control.Monad          (forM_, unless, when, (<=<))
import           Control.Monad.Catch    (MonadThrow (throwM))
import           Control.Monad.Extra    (unlessM, whenJust, whenM)
import           Data.Acid              (Query, Update, liftQuery)
import qualified Data.Foldable          as F
import qualified Data.HashMap.Strict    as HM hiding (HashMap)
import           Data.HashSet           (HashSet)
import qualified Data.HashSet           as HS hiding (HashSet)
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IM hiding (IntMap)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M hiding (Map)
import           Data.Maybe             (fromJust, fromMaybe, mapMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as S hiding (Set)
import           Formatting             (build, sformat, (%))

import           RSCoin.Core            (AddrId, Address (..), HBlock (..),
                                         PeriodId, PublicKey, Signature,
                                         Transaction (..), Utxo,
                                         computeOutputAddrids,
                                         validateSignature, validateTxSum, verify)
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

data Storage = Storage
    { -- | Pool of trasactions to be signed, already collected signatures.
      -- @TODO: what an ugly type ;(
      _txPool                 :: !(Map Address (Map Transaction (Map Address (Signature Transaction))))

      -- | Mapping between addrid and all pairs (addr, transaction),
      -- being kept in `txPool`, such that addrid serve as an input for transaction.
    , _txPoolAddrIds          :: !(Map AddrId (Set (Address, Transaction)))

      -- | Mapping between address and a set of unspent addrids, owned by it.
    , _unspentAddrIds         :: !(Map Address (Set AddrId))

      -- | Mapping from newly allocated multisignature addresses. This Map is
      -- used only during multisignature address allocation process.
    , _allocationStrategyPool :: !(Map MSAddress AllocationInfo)

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
    , _periodStats            :: !(Map Address Int)

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
    { _txPool                 = M.empty
    , _txPoolAddrIds          = M.empty
    , _unspentAddrIds         = M.empty
    , _allocationStrategyPool = M.empty
    , _discardMSAddresses     = IM.empty
    , _discardTransactions    = IM.empty
    , _allocationEndurance    = defaultAllocationEndurance
    , _transactionEndurance   = defaultTransactionEndurance
    , _periodStats            = M.empty
    , _addresses              = M.empty
    , _utxo                   = M.empty
    , _masterKeys             = []
    , _periodId               = -1
    }

{- Utility non exported functions -}

ifNotEmpty :: Foldable t => t a -> Maybe (t a)
ifNotEmpty s | F.null s  = Nothing
             | otherwise = Just s

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
        M.insertWith (\new old -> min (old + new) allocationAttemptsLimit) userAddr 1
    currentAttemtps <- uses periodStats $ fromJust . M.lookup userAddr
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

      mMSAddressInfo <- uses allocationStrategyPool $ M.lookup msAddr
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
             allocationStrategyPool %= M.delete adress

---------------
-- transactions
---------------

-- | Erase occurrences published (address, transaction) from storage
forgetAddrTx :: Address -> Transaction -> Update Storage ()
forgetAddrTx addr tx = do
    txPool %= M.update (ifNotEmpty . M.delete tx) addr
    txPoolAddrIds %=
        \m -> foldr (M.update $ ifNotEmpty . S.delete (addr, tx)) m (txInputs tx)

-- | Receives tx, addr, (addr, sig) pair, checks validity and
-- publishes (tx, addr) to storage, adds (addr, sig) to list of
-- already collected for particular (tx, addr) pair.
addSignedTransaction :: Transaction
                     -> Address
                     -> (Address, Signature Transaction)
                     -> Update Storage ()
addSignedTransaction tx addr (sigAddr,sig) = do
    checkTransactionValidity
    checkAddrIdsKnown
    checkAddrRelativeToTx
    checkSigRelativeToAddr
    txMap <- fromMaybe M.empty . M.lookup addr <$> use txPool
    when (tx `M.notMember` txMap) $ txPoolAddrIds %= updateTxPoolAddrIds

    pId <- use periodId
    discardTransactions %= IM.alter (Just . (tx :) . fromMaybe []) pId
    txPool %=
        M.insert
            addr
            (M.alter (Just . M.insert sigAddr sig . fromMaybe M.empty) tx txMap)
  where
    updateTxPoolAddrIds m = foldr (M.alter f) m $ txInputs tx
      where
        f = Just . S.insert (addr, tx) . fromMaybe S.empty
    checkTransactionValidity =
        unless (validateTxSum tx) $
        throwM $
        NEInvalidArguments $
        sformat ("Transaction doesn't pass valitidy check: " % build) tx
    -- Throws error if addrid isn't known (with corresponding label)
    -- User should repeat transaction after some timeout
    checkAddrIdsKnown = do
        u <- use utxo
        unless (all (`M.member` u) (txInputs tx)) $
            use periodId >>= throwM . NEAddrIdNotInUtxo
    checkAddrRelativeToTx = do
        s <- fromMaybe S.empty . M.lookup addr <$> use unspentAddrIds
        unless (any (`S.member` s) (txInputs tx)) $
            throwM NEAddrNotRelativeToTx
    checkSigRelativeToAddr = do
        strategy <- liftQuery (getStrategy addr)
        case strategy of
            DefaultStrategy ->
                throwM $ NEStrategyNotSupported "DefaultStrategy"
            MOfNStrategy _ addrs ->
                when (sigAddr `notElem` addrs) $
                throwM $ NEUnrelatedSignature "in multi transaction"
        unless (validateSignature sig sigAddr tx) $ throwM NEInvalidSignature

--------------------
-- handle period end
--------------------

-- | Announce HBlocks, not yet known to Notary.
announceNewPeriods :: PeriodId -- ^ periodId of latest hblock
                   -> [HBlock] -- ^ blocks, head corresponds to the latest block
                   -> Update Storage ()
announceNewPeriods pId' blocks = do
    pId <- use periodId
    mapM_ announceNewPeriod $ reverse $ take (pId' - pId) blocks
    periodId .= pId'

    -- @TODO: remove duplcated code
    allocAliveInterval <- use allocationEndurance
    txAliveInterval    <- use transactionEndurance

    -- remove autdated allocations
    unlessM (uses discardMSAddresses IM.null) $ do
        (oldestSavedPid, _) <- uses discardMSAddresses IM.findMin
        let deleteLookup = const $ const Nothing

        forM_ [oldestSavedPid .. pId - allocAliveInterval] $ \oldPid -> do
            (mMsList, newDiscard) <- uses discardMSAddresses
                $ IM.updateLookupWithKey deleteLookup oldPid
            discardMSAddresses .= newDiscard -- @TODO: optimize and set only once

            whenJust mMsList $ \msList -> forM_ msList $ \msAddr ->
                allocationStrategyPool %= M.delete msAddr

    -- remove outdated transactions
    unlessM (uses discardTransactions IM.null) $ do
        (oldestSavedPid, _) <- uses discardTransactions IM.findMin
        let deleteLookup = const $ const Nothing

        forM_ [oldestSavedPid .. pId - txAliveInterval] $ \oldPid -> do
            (mTxList, newDiscard) <- uses discardTransactions
                $ IM.updateLookupWithKey deleteLookup oldPid
            discardTransactions .= newDiscard -- @TODO: optimize and set only once

            -- this shit just makes me really sad :(
            whenJust mTxList $ \txList -> forM_ txList $ \tx -> do
                allMSAddrs <- uses txPool M.keys
                forM_ allMSAddrs $ \msAddr ->
                    txPool %= M.update (ifNotEmpty . M.delete tx) msAddr


announceNewPeriod :: HBlock -> Update Storage ()
announceNewPeriod HBlock{..} = do
    addresses %= M.union hbAddresses
    forM_ (concatMap txInputs hbTransactions) processTxIn
    forM_ (concatMap computeOutputAddrids hbTransactions) $ uncurry processTxOut
    periodStats .= M.empty
  where
    processTxIn addrId = do
        addrM <- M.lookup addrId <$> use utxo
        whenJust addrM $ processTxIn' addrId
    processTxIn' addrId addr = do
        utxo %= M.delete addrId
        unspentAddrIds %= M.update (ifNotEmpty . S.delete addrId) addr
        txAddrPairs <- fromMaybe S.empty . M.lookup addrId <$> use txPoolAddrIds
        txPoolAddrIds %= M.delete addrId
        forM_ txAddrPairs $ uncurry forgetAddrTx
    processTxOut addrId addr = do
        utxo %= M.insert addrId addr
        unspentAddrIds %= M.alter (Just . S.insert addrId . fromMaybe S.empty) addr

-- =============
-- QUERY SECTION
-- =============

-----------------
-- ms alloccation
-----------------

outdatedAllocs :: Query Storage (IntMap [MSAddress])
outdatedAllocs = view discardMSAddresses

queryMSAddressesHelper
    :: Lens' Storage (Map MSAddress info)
    -> (info -> Bool)
    -> (info -> meta)
    -> Query Storage [(MSAddress, meta)]
queryMSAddressesHelper poolLens selector mapper =
    view
    $ poolLens
    . to (M.filter selector)
    . to (M.map mapper)
    . to M.assocs

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

-- | By given (tx, addr) get list of collected signatures (or empty list if (tx, addr)
-- is not registered/already removed from Notary). Read-only method.
getSignatures :: Transaction -> Address -> Query Storage [(Address, Signature Transaction)]
getSignatures tx addr = maybe [] M.assocs . (M.lookup tx <=< M.lookup addr) <$> view txPool

-- | Get last known periodId of Notary (interface for bank).
getPeriodId :: Query Storage PeriodId
getPeriodId = view periodId

-- | Collect all pending multisignature transactions which have one of
-- party is a member of given list.
-- @TODO: replace [Address] with @HashSet Address@ for faster checks
pollPendingTxs :: [Address] -> Query Storage [Transaction]
pollPendingTxs parties = do
    curTxPool      <- view txPool
    curUtxo        <- view utxo
    let pendingTxs  = M.elems curTxPool >>= M.keys
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
