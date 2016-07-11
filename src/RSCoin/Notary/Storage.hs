{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Storage for Notary's data.

module RSCoin.Notary.Storage
        ( Storage
        , emptyNotaryStorage
        , getSignatures
        , acquireSignatures
        , addSignedTransaction
        , announceNewPeriods
        , getPeriodId
        , pollTransactions
        ) where

import           Control.Exception   (throw)
import           Control.Lens        (makeLenses, use, view, (%=), (.=))
import           Control.Monad       (forM_, when, (<=<))
import           Control.Monad.Catch (MonadThrow (throwM))

import           Data.Acid           (Query, Update, liftQuery)
import qualified Data.Foldable       as F
import           Data.Map            (Map)
import qualified Data.Map            as M hiding (Map)
import           Data.Maybe          (fromJust, fromMaybe, isJust)
import           Data.Set            (Set)
import qualified Data.Set            as S hiding (Set)

import           RSCoin.Core         (AddrId, Address, AddressToStrategyMap,
                                      HBlock (..),
                                      PeriodId, Signature,
                                      Strategy (..), Transaction (..), Utxo,
                                      computeOutputAddrids, isStrategyCompleted,
                                      validateSignature)
import           RSCoin.Notary.Error (NotaryError (..))

data Storage = Storage
    { -- | Pool of trasactions to be signed, already collected signatures.
      _txPool         :: Map Address (Map Transaction (Map Address Signature))

      -- | Mapping between addrid and all pairs (addr, transaction),
      -- being kept in `txPool`, such that addrid serve as an input for transaction.
    , _txPoolAddrIds  :: Map AddrId (Set (Address, Transaction))

      -- | Mapping between address and a set of unspent addrids, owned by it.
    , _unspentAddrIds :: Map Address (Set AddrId)

      -- | Mapping from newly allocated multisignature address to set of
      -- parties. This Map is used only during multisignature
      -- address allocation process.
    , _allocationPool :: Map Address (Set Address)

      -- | Non-default addresses, registered in system (published to bank).
    , _addresses      :: AddressToStrategyMap

      -- | Mapping between addrid and address.
    , _utxo           :: Utxo

      -- | Last periodId, known to Notary.
    , _periodId       :: PeriodId
    } deriving (Show)

$(makeLenses ''Storage)

emptyNotaryStorage :: Storage
emptyNotaryStorage =
    Storage
    { _txPool         = M.empty
    , _txPoolAddrIds  = M.empty
    , _unspentAddrIds = M.empty
    , _allocationPool = M.empty
    , _addresses      = M.empty
    , _utxo           = M.empty
    , _periodId       = -1
    }

-- Erase occurrences published (address, transaction) from storage
forgetAddrTx :: Address -> Transaction -> Update Storage ()
forgetAddrTx addr tx = do
  txPool %= M.update (ifNotEmpty . M.delete tx) addr
  txPoolAddrIds %= \m -> foldr (M.update $ ifNotEmpty . S.delete (addr, tx)) m (txInputs tx)

ifNotEmpty :: Foldable t => t a -> Maybe (t a)
ifNotEmpty s | F.null s  = Nothing
             | otherwise = Just s

getStrategy :: Address -> Update Storage Strategy
getStrategy addr = fromMaybe DefaultStrategy . M.lookup addr <$> use addresses

instance MonadThrow (Update s) where
    throwM = throw

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
                 throwM NEUnrelatedSignature
        when (not $ validateSignature sig sigAddr tx) $
          throwM NEInvalidSignature

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
