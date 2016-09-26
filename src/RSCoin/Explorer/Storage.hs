{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Explorer data storage.

module RSCoin.Explorer.Storage
       ( Storage
       , mkStorage

         -- | Queries
       , Query
       , getAddressBalance
       , getAddressTxNumber
       , getAddressTransactions
       , getExpectedPeriodId
       , getHBlocksExtended
       , getHBlockExtended
       , getTx
       , getTxExtended
       , getTxExtensions
       , getTxsGlobal
       , isAddressKnown
       , isTransactionKnown

         -- | Updates
       , Update
       , ExceptUpdate
       , addHBlock

       ) where

import           Control.Applicative      (liftA2, (<|>))
import           Control.Lens             (at, ix, makeLenses, makeLensesFor, preview,
                                           use, view, views, (%=), (+=), (.=), _Just)
import           Control.Monad            (forM_, unless)
import           Control.Monad.Catch      (MonadThrow (throwM))
import           Control.Monad.Extra      (ifM, whenJust)
import           Control.Monad.Reader     (MonadReader, Reader, runReader)
import           Control.Monad.State      (MonadState, gets)
import           Data.Foldable            (foldr')
import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import qualified Data.IntMap.Strict       as I
import           Data.List                (genericLength)
import           Data.Maybe               (catMaybes, fromMaybe)
import           Data.SafeCopy            (base, deriveSafeCopy)
import qualified Data.Vector              as V
import           Formatting               (build, sformat, (%))

import           Serokell.Util.Common     (enumerate, indexedSubList)

import qualified RSCoin.Core              as C

import           RSCoin.Explorer.Error    (ExplorerError (..))
import           RSCoin.Explorer.Extended (CoinsMapExtended, HBlockExtended, Timestamp,
                                           TransactionExtended, TransactionExtension (..),
                                           cmeTotal, mkCoinsMapExtended, mkHBlockExtended,
                                           mkTransactionExtension)

$(makeLensesFor
      [("wmValue", "wmVal"), ("wmMetadata", "wmExtension")]
      ''C.WithMetadata)

$(makeLensesFor [("hbTransactions", "hbTransactionsL")] ''C.HBlock)

-- | TransactionIndex consists of two indices: the first one is the
-- index of block containing this transaction, the second one is the
-- index of transaction in this block. It's implementation detail of
-- this storage.
data TransactionIndex = TransactionIndex
    { tiPeriod :: !C.PeriodId
    , tiIdx    :: !Word
    } deriving (Show)

$(deriveSafeCopy 0 'base ''TransactionIndex)

data AddressData = AddressData
    { _adBalance      :: !CoinsMapExtended
    , _adTransactions :: ![TransactionIndex]
    }

mkAddressData :: AddressData
mkAddressData =
    AddressData
    { _adBalance = mkCoinsMapExtended C.zeroCoinsMap
    , _adTransactions = mempty
    }

$(makeLenses ''AddressData)

$(deriveSafeCopy 0 'base ''AddressData)

data Storage = Storage
    { -- | State of all addresses ever seen by this explorer.
      _addresses       :: !(HM.HashMap C.Address AddressData)
      -- | Extended higher level blocks received by this explorer from
      -- the very beginning.
    , _hBlocks         :: !(V.Vector HBlockExtended)
      -- | Extensions of transactions. There is a trade-off between
      -- memory usage and request processing time. They are not very
      -- big, so we store them instead of recalculating every time.
    , _txExtensions    :: !(V.Vector (V.Vector TransactionExtension))
      -- | Mapping from transaction id to index of actual transaction
      -- with this id. Contains all transactions ever seen by this
      -- explorer. See TransactionIndex documentation also.
    , _transactionsMap :: !(HM.HashMap C.TransactionId TransactionIndex)
      -- | All emission hashes from the very beginning. It's only used
      -- for checking purposes.
    , _emissionHashes  :: !(HS.HashSet C.TransactionId)
    }

$(makeLenses ''Storage)

$(deriveSafeCopy 0 'base ''Storage)

-- | Make initial (empty) storage.
mkStorage :: Storage
mkStorage =
    Storage
    { _addresses = mempty
    , _hBlocks = mempty
    , _txExtensions = mempty
    , _transactionsMap = mempty
    , _emissionHashes = mempty
    }

type Query a = forall m. MonadReader Storage m => m a

addTimestamp :: Query a -> Query (C.PeriodId, a)
addTimestamp q = (,) <$> getExpectedPeriodId <*> q

-- | Get amount of coins (as CoinsMap) available from given
-- address. Result is timestamped with id of ongoing period.
getAddressBalance :: C.Address -> Query (C.PeriodId, CoinsMapExtended)
getAddressBalance addr =
    addTimestamp $
    views
        (addresses . at addr)
        (maybe (mkCoinsMapExtended C.zeroCoinsMap) (view adBalance))

-- | Get number of transactions refering to given address. Result is
-- timestamped with id of ongoing period.
getAddressTxNumber :: C.Address -> Query (C.PeriodId, Word)
getAddressTxNumber addr =
    addTimestamp $
    views (addresses . at addr) (maybe 0 (genericLength . view adTransactions))

-- | Get subset of transactions referring to given address. Index of
-- the most recent transaction is 0. Returns indexed list of
-- transactions in range [lo, min (hi, txNum)). Result is timestamped
-- with id of ongoing period.
getAddressTransactions :: C.Address
                       -> (Word, Word)
                       -> Query (C.PeriodId, [(Word, TransactionExtended)])
getAddressTransactions addr indices =
    addTimestamp $
    indexedSubList indices . catMaybes <$>
    (mapM txIdxToTxExtended =<<
     views (addresses . at addr) (maybe [] (view adTransactions)))

-- | Get PeriodId of expected HBlock.
getExpectedPeriodId :: Query C.PeriodId
getExpectedPeriodId = views hBlocks length

txIdxToTx :: TransactionIndex -> Query (Maybe C.Transaction)
txIdxToTx TransactionIndex {..} =
    preview $
    hBlocks . ix tiPeriod . wmVal . hbTransactionsL . ix (fromIntegral tiIdx)

txIdxToTxExtension :: TransactionIndex -> Query (Maybe TransactionExtension)
txIdxToTxExtension TransactionIndex {..} =
    preview $ txExtensions . ix tiPeriod . ix (fromIntegral tiIdx)

txIdxToTxExtended :: TransactionIndex -> Query (Maybe TransactionExtended)
txIdxToTxExtended idx =
    liftA2 C.WithMetadata <$> txIdxToTx idx <*> txIdxToTxExtension idx

-- | Get transaction with given id (if it can be found).
getTx :: C.TransactionId -> Query (Maybe C.Transaction)
getTx = fmap (fmap C.wmValue) . getTxExtended

-- | Get extended transaction with given id (if it can be found).
getTxExtended :: C.TransactionId -> Query (Maybe TransactionExtended)
getTxExtended i =
    maybe (pure Nothing) txIdxToTxExtended =<< view (transactionsMap . at i)

-- | Get extensions of all transactions in given period.
getTxExtensions :: C.PeriodId -> Query [TransactionExtension]
getTxExtensions i =
    V.toList . fromMaybe mempty <$> preview (txExtensions . ix i)

-- | Get transactions within given range from global history.
-- 0-th transaction is the latest one.
getTxsGlobal :: (Word, Word) -> Query (C.PeriodId, [(Word, TransactionExtended)])
getTxsGlobal range =
    addTimestamp $
    (indexedSubList range . catMaybes . V.toList) <$>
    (mapM txIdxToTxExtended =<< getRecentTxsIndices (snd range))

getRecentTxsIndices :: Word -> Query (V.Vector TransactionIndex)
getRecentTxsIndices n = do
    blocks <- view hBlocks
    let blocksNumber = V.length blocks
        blocksTxsLengths =
            fmap (genericLength . C.hbTransactions . C.wmValue) blocks
    pure $
        V.reverse . foldr' step [] . V.zip [0 .. blocksNumber] $
        blocksTxsLengths
  where
    step (blkIdx, blkTxsLen) res
        | blkTxsLen == 0 || V.length res >= fromIntegral n = res
        | otherwise =
            fmap (TransactionIndex blkIdx) [0 .. blkTxsLen - 1] `mappend` res

-- | Get indexed list of extended HBlocks in given range.
getHBlocksExtended :: (C.PeriodId, C.PeriodId) -> Query [(C.PeriodId, HBlockExtended)]
getHBlocksExtended indices = indexedSubList indices . V.toList <$> view hBlocks

-- | Get extended HBlock with given id (if it exists).
getHBlockExtended :: C.PeriodId -> Query (Maybe HBlockExtended)
getHBlockExtended blkId = (V.!? blkId) <$> view hBlocks

-- | Returns True iff Explorer is aware of this address.
isAddressKnown :: C.Address -> Query Bool
isAddressKnown addr = views addresses (HM.member addr)

-- | Returns True iff Explorer is aware of this transaction.
isTransactionKnown :: C.TransactionId -> Query Bool
isTransactionKnown i = views transactionsMap (HM.member i)

type Update a = forall m. MonadState Storage m => m a
type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

-- TODO: maybe move somewhere
readerToState
    :: MonadState s m
    => Reader s a -> m a
readerToState = gets . runReader

getTxChecked ::
    forall m.
    (MonadThrow m, MonadState Storage m)
    => C.PeriodId
    -> HM.HashMap C.TransactionId C.Transaction
    -> C.TransactionId
    -> m (Maybe C.Transaction)
getTxChecked pId newTxs txId = do
    oldTx <- readerToState $ getTx txId
    let msg = sformat ("Invalid transaction id seen: " % build)txId
        newTx = HM.lookup txId newTxs
        tx = oldTx <|> newTx
    case tx of
        Nothing ->
            ifM
                (HS.member txId <$> use emissionHashes)
                (pure Nothing)
                (throwM $ EEIncorrectBlock pId msg)
        justTx -> pure justTx

-- | Modify storage by adding given higher-level block. Period
-- identifier is required to check that given HBlock is the next after
-- last applied block.
addHBlock :: C.PeriodId -> C.WithMetadata C.HBlock C.HBlockMetadata -> ExceptUpdate ()
addHBlock pId blkWithMeta@(C.WithMetadata C.HBlock {..} C.HBlockMetadata {..}) = do
    expectedPid <- readerToState getExpectedPeriodId
    unless (expectedPid == pId) $
        throwM
            EEPeriodMismatch
            { pmExpectedPeriod = expectedPid
            , pmReceivedPeriod = pId
            }
    forM_ hbmEmission (\em -> emissionHashes %= HS.insert em)
    mapM_ (addTxToMap pId) $ enumerate hbTransactions
    let newTxs = HM.fromList $ map (\tx -> (C.hash tx, tx)) hbTransactions
    extendedBlk <- mkHBlockExtended pId blkWithMeta (getTxChecked pId newTxs)
    hBlocks %= flip V.snoc extendedBlk
    extensions <- mapM (mkTxExtension newTxs pId hbmTimestamp) hbTransactions
    txExtensions %= flip V.snoc (V.fromList extensions)
    let extendedTxs = zipWith C.WithMetadata hbTransactions extensions
    mapM_ applyTxToAddresses $
        zip (map (TransactionIndex pId) [0 ..]) extendedTxs

addTxToMap :: C.PeriodId -> (Word, C.Transaction) -> Update ()
addTxToMap pId (txIdx, tx) = transactionsMap . at (C.hash tx) .= Just index
  where
    index =
        TransactionIndex
        { tiPeriod = pId
        , tiIdx = txIdx
        }

mkTxExtension
    :: HM.HashMap C.TransactionId C.Transaction
    -> C.PeriodId
    -> Timestamp
    -> C.Transaction
    -> ExceptUpdate TransactionExtension
mkTxExtension newTxs pId timestamp = mkTransactionExtension pId timestamp getTxs
  where
    getTxs = getTxChecked pId newTxs

applyTxToAddresses :: (TransactionIndex, TransactionExtended) -> Update ()
applyTxToAddresses (i, C.WithMetadata C.Transaction {..} TransactionExtension {..}) = do
    mapM_ (applyTxInput i) $ zip txInputs teInputAddresses
    mapM_ (applyTxOutput i) txOutputs

applyTxInput :: TransactionIndex -> (C.AddrId, Maybe C.Address) -> Update ()
applyTxInput i ((_, _, c), mAddr) = whenJust mAddr $ changeAddressData i (-c)

applyTxOutput :: TransactionIndex -> (C.Address, C.Coin) -> Update ()
applyTxOutput i (addr,c) = changeAddressData i c addr

changeAddressData :: TransactionIndex -> C.Coin -> C.Address -> Update ()
changeAddressData tx c addr = do
    ensureAddressExists addr
    addresses . at addr . _Just . adTransactions %= (tx :)
    let aBalance = addresses . at addr . _Just . adBalance
    aBalance . wmVal  %= I.insertWith (+) (C.getColor $ C.coinColor c) c
    aBalance . wmExtension . cmeTotal += C.coinAmount c

ensureAddressExists :: C.Address -> Update ()
ensureAddressExists addr =
    addresses %= HM.alter (Just . fromMaybe mkAddressData) addr
