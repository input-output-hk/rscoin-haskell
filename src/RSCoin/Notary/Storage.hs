{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Storage for Notary's data.

module RSCoin.Notary.Storage
        ( Storage
        , acquireSignatures
        , addSignedTransaction
        , allocateMSAddress
        , announceNewPeriods
        , emptyNotaryStorage
        , getPeriodId
        , getSignatures
        , pollTransactions
        , removeCompleteMSAddresses
        , queryAllMSAdresses
        , queryCompleteMSAdresses
        ) where

import           Control.Exception   (throw)
import           Control.Lens        (makeLenses, to, use, uses, view, (%=),
                                      (.=))
import           Control.Monad       (forM_, unless, when, (<=<))
import           Control.Monad.Catch (MonadThrow (throwM))

import           Data.Acid           (Query, Update, liftQuery)
import           Data.Bifunctor      (second)
import qualified Data.Foldable       as F
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M hiding (Map)
import           Data.Maybe          (fromJust, fromMaybe, isJust)
import           Data.Set            (Set)
import qualified Data.Set            as S hiding (Set)

import           RSCoin.Core         (AddrId, Address (..),
                                      HBlock (..),
                                      PeriodId, PublicKey, Signature,
                                      Transaction (..),  Utxo,
                                      computeOutputAddrids,
                                      notaryMSAttemptsLimit, validateSignature,
                                      verify, verifyChain)
import           RSCoin.Core.Strategy (AddressToTxStrategyMap, AllocationParty (..),
                                       AllocationStrategy (..), TxStrategy (..),
                                       isStrategyCompleted)
import           RSCoin.Core.Trusted  (bankColdPublic, chainRootPKs)
import           RSCoin.Notary.Error (NotaryError (..))

data Storage = Storage
    { -- | Pool of trasactions to be signed, already collected signatures.
      _txPool         :: Map Address (Map Transaction (Map Address Signature))

      -- | Mapping between addrid and all pairs (addr, transaction),
      -- being kept in `txPool`, such that addrid serve as an input for transaction.
    , _txPoolAddrIds  :: Map AddrId (Set (Address, Transaction))

      -- | Mapping between address and a set of unspent addrids, owned by it.
    , _unspentAddrIds :: Map Address (Set AddrId)

      -- | Mapping from newly allocated multisignature address to pair of sets of
      -- parties. First element is resulted party, second - is current. This Map is
      -- used only during multisignature address allocation process for 'UserStrategy'.
    , _userStrategyPool :: Map Address (TxStrategy, Set Address)

      -- | Same as '_userStrategyPool' but this Map is used for 'SharedStrategy'.
    , _sharedStrategyPool :: Map AllocationParty Address

      -- | Number of attempts for user per period to allocate multisig address.
    , _periodStats    :: Map Address Int

      -- | Non-default addresses, registered in system (published to bank).
    , _addresses      :: AddressToTxStrategyMap

      -- | Mapping between addrid and address.
    , _utxo           :: Utxo

      -- | Last periodId, known to Notary.
    , _periodId       :: PeriodId
    } deriving (Show)

$(makeLenses ''Storage)

emptyNotaryStorage :: Storage
emptyNotaryStorage =
    Storage
    { _txPool             = M.empty
    , _txPoolAddrIds      = M.empty
    , _unspentAddrIds     = M.empty
    , _userStrategyPool   = M.empty
    , _sharedStrategyPool = M.empty
    , _periodStats        = M.empty
    , _addresses          = M.empty
    , _utxo               = M.empty
    , _periodId           = -1
    }

-- Erase occurrences published (address, transaction) from storage
forgetAddrTx :: Address -> Transaction -> Update Storage ()
forgetAddrTx addr tx = do
  txPool %= M.update (ifNotEmpty . M.delete tx) addr
  txPoolAddrIds %= \m -> foldr (M.update $ ifNotEmpty . S.delete (addr, tx)) m (txInputs tx)

ifNotEmpty :: Foldable t => t a -> Maybe (t a)
ifNotEmpty s | F.null s  = Nothing
             | otherwise = Just s

getStrategy :: Address -> Update Storage TxStrategy
getStrategy addr = fromMaybe DefaultStrategy . M.lookup addr <$> use addresses

instance MonadThrow (Update s) where
    throwM = throw

-- | Throws NEBlocked if user reaches limit of attempts (DDOS protection).
guardMaxAttemps :: Address -> Update Storage ()
guardMaxAttemps userAddr = do
    periodStats %= M.insertWith (\new old -> min (old + new) notaryMSAttemptsLimit) userAddr 1
    currentAttemtps <- uses periodStats $ fromJust . M.lookup userAddr
    when (currentAttemtps >= notaryMSAttemptsLimit) $ throwM NEBlocked

-- | Receives tx, addr, (addr, sig) pair, checks validity and publishes (tx, addr) to storage,
-- adds (addr, sig) to list of already collected for particular (tx, addr) pair.
addSignedTransaction :: Transaction -> Address -> (Address, Signature) -> Update Storage ()
addSignedTransaction tx addr sg@(sigAddr, sig) = do
    -- @TODO check transaction correctness, i.e. equality of sums and etc.
    checkAddrIdsKnown
    checkAddrRelativeToTx
    checkSigRelativeToAddr
    txMap <- fromMaybe M.empty . M.lookup addr <$> use txPool
    when (not $ tx `M.member` txMap) $
      txPoolAddrIds %= updateTxPoolAddrIds
    txPool %= M.insert addr (M.alter (Just. uncurry M.insert sg . fromMaybe M.empty) tx txMap)
  where
    updateTxPoolAddrIds m = foldr (M.alter f) m $ txInputs tx
      where f = Just . S.insert (addr, tx) . fromMaybe S.empty
    -- Throws error if addrid isn't known (with corresponding label)
    -- User should repeat transaction after some timeout
    checkAddrIdsKnown = do
      u <- use utxo
      when (not $ all (`M.member` u) (txInputs tx)) $
         use periodId >>= throwM . NEAddrIdNotInUtxo
    checkAddrRelativeToTx = do
      s <- fromMaybe S.empty . M.lookup addr <$> use unspentAddrIds
      when (not $ any (`S.member` s) (txInputs tx)) $
         throwM NEAddrNotRelativeToTx
    checkSigRelativeToAddr = do
        strategy <- getStrategy addr
        case strategy of
          DefaultStrategy
            -> throwM $ NEStrategyNotSupported "DefaultStrategy"
          MOfNStrategy _ addrs
            -> when (not $ any (sigAddr ==) addrs) $
                 throwM $ NEUnrelatedSignature "in multi transaction"
        when (not $ validateSignature sig sigAddr tx) $
          throwM NEInvalidSignature

-- | Allocate new multisignature address by chosen strategy and
-- given chain of certificates.
allocateMSAddress
    :: Address                  -- ^ New multisig address itself
    -> AllocationStrategy       -- ^ Strategy for MS address allocation
    -> (Address, Signature)     -- ^ *Address of party* who adds address and signature of 'Address'
    -> [(Signature, PublicKey)] -- ^ Certificate chain to authorize *address of party*.
                                -- Head is cert, given by *root* (Attain)
    -> Update Storage ()
allocateMSAddress msAddr allocStrat (partyAddr@(Address partyPK), partySig) chain
    -- too many checks :( I wish I know which one we shouldnt check
    | partyPK /= snd (last chain) = throwM $ -- @TODO: what if infinite list is given?
        NEInvalidChain "last address of chain should be party address"
    | any (`verifyChain` chain) chainRootPKs = throwM $
        NEInvalidChain "none of root pk's is fit for validating"
    | otherwise = case allocStrat of
        -- case when strategy is allocated only among users
        UserStrategy m parties -> do
            unless (verify partyPK partySig partyAddr) $
                throwM $ NEUnrelatedSignature "user-strategy not signed with user pk"
            when (partyAddr `S.notMember` parties) $
                throwM $ NEInvalidArguments "party address not in set of addresses"
            when (m <= 0) $
                throwM $ NEInvalidArguments "required number of signatures should be positive"
            when (m > S.size parties) $
                throwM $ NEInvalidArguments "required number of signatures is greater then party size"
            guardMaxAttemps partyAddr

            mMSAddressSets <- uses userStrategyPool $ M.lookup msAddr
            case mMSAddressSets of
                Nothing ->
                    -- !!! WARNING !!! EASY OutOfMemory HERE!
                    userStrategyPool %= M.insert msAddr (MOfNStrategy m parties, S.singleton partyAddr)
                Just (strategy@(MOfNStrategy resM resultParties), currentParties) -> do
                    when (partyAddr `S.notMember` resultParties) $
                        throwM $ NEInvalidArguments "party set exists, but you is not a member!"
                    when (resultParties /= parties || m /= resM) $
                        throwM $ NEInvalidArguments "result stratey is not equal to yours"
                    userStrategyPool %= M.insert msAddr (strategy, S.insert partyAddr currentParties)
                Just (DefaultStrategy, _) ->
                    error "There is a DefaultStrategy in userStrategyPool"

        -- MS address allocation with User and Trusted party
        SharedStrategy tParty -> do
            case tParty of
                Trusted -> unless (verify bankColdPublic partySig partyAddr) $
                    throwM $ NEUnrelatedSignature "shared-strategy not signed by Bank cold"
                User    -> do
                    unless (verify partyPK partySig partyAddr) $
                        throwM $ NEUnrelatedSignature "shared-strategy not signed by User pk"
                    guardMaxAttemps partyAddr

            sharedStrategyPool %= M.insert tParty partyAddr


queryAllMSAdresses :: Query Storage [(Address, (TxStrategy, Set Address))]
queryAllMSAdresses = view $ userStrategyPool . to M.assocs

queryCompleteMSAdresses :: Query Storage [(Address, TxStrategy)]
queryCompleteMSAdresses =
    view
    $ userStrategyPool
    . to (M.filter
        (\(MOfNStrategy _ resultParties, currentParties)  -- @TODO: remove non-exhaustive warning
            -> S.size resultParties == S.size currentParties
        ))
    . to M.assocs
    . to (map $ second fst)

removeCompleteMSAddresses :: [Address] -> Update Storage ()
removeCompleteMSAddresses completeAddrs = forM_ completeAddrs $ \adress ->
    userStrategyPool %= M.delete adress

-- | By given (tx, addr) retreives list of collected signatures.
-- If list is complete enough to complete strategy, (tx, addr) pair
-- and all corresponding data occurrences get removed from Storage.
acquireSignatures :: Transaction -> Address -> Update Storage [(Address, Signature)]
acquireSignatures tx addr = do
    sgs <- liftQuery (getSignatures tx addr)
    strategy <- getStrategy addr
    when (isStrategyCompleted strategy addr sgs tx) $
        forgetAddrTx addr tx
    return sgs

-- | By given (tx, addr) get list of collected signatures (or empty list if (tx, addr)
-- is not registered/already removed from Notary). Read-only method.
getSignatures :: Transaction -> Address -> Query Storage [(Address, Signature)]
getSignatures tx addr = maybe [] M.assocs . (M.lookup tx <=< M.lookup addr) <$> view txPool

-- | Get last known periodId of Notary (interface for bank).
getPeriodId :: Query Storage PeriodId
getPeriodId = view periodId

-- | Announce HBlocks, not yet known to Notary.
announceNewPeriods :: PeriodId -- ^ periodId of latest hblock
                   -> [HBlock] -- ^ blocks, head corresponds to the latest block
                   -> Update Storage ()
announceNewPeriods pId' blocks = do
    pId <- use periodId
    mapM_ announceNewPeriod $ reverse $ take (pId' - pId) blocks
    periodId .= pId'

announceNewPeriod :: HBlock -> Update Storage ()
announceNewPeriod HBlock{..} = do
      addresses %= M.union hbAddresses
      forM_ (concatMap txInputs hbTransactions) processTxIn
      forM_ (concatMap computeOutputAddrids hbTransactions) $ uncurry processTxOut
      periodStats .= M.empty
  where
    processTxIn addrId = do
        addrM <- M.lookup addrId <$> use utxo
        when (isJust addrM) $ processTxIn' addrId (fromJust addrM)
    processTxIn' addrId addr = do
        utxo %= M.delete addrId
        unspentAddrIds %= M.update (ifNotEmpty . S.delete addrId) addr
        txAddrPairs <- fromMaybe S.empty . M.lookup addrId <$> use txPoolAddrIds
        txPoolAddrIds %= M.delete addrId
        forM_ txAddrPairs $ uncurry forgetAddrTx
    processTxOut addrId addr = do
        utxo %= M.insert addrId addr
        unspentAddrIds %= M.alter (Just . S.insert addrId . fromMaybe S.empty) addr

-- @TODO implement
pollTransactions :: [Address] -> Query Storage [(Address, [(Transaction, [(Address, Signature)])])]
pollTransactions _ = return []
