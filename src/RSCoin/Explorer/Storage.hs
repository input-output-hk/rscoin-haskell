{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Explorer data storage.

module RSCoin.Explorer.Storage
       ( Storage
       , mkStorage

         -- | Queries
       , Query
       , getAddressBalance
       , getAddressInterestingTransactions
       , getAddressTxNumber
       , getAddressTransactions
       , getExpectedPeriodId
       , getInterestingTxsGlobal
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
                                           use, view, views, (%%=), (%=), (+=), (.=),
                                           _Just)
import           Control.Monad            (filterM, foldM, forM_, unless, when)
import           Control.Monad.Catch      (MonadThrow (throwM))
import           Control.Monad.Extra      (ifM, whenJust)
import           Control.Monad.Reader     (MonadReader, Reader, runReader)
import           Control.Monad.State      (MonadState, gets)
import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import qualified Data.IntMap.Strict       as I
import           Data.List                (genericLength)
import           Data.Maybe               (catMaybes, fromMaybe)
import           Data.Monoid              (Any (..))
import           Data.SafeCopy            (base, deriveSafeCopy)
import qualified Data.Vector              as V
import           Formatting               (build, sformat, (%))

import           Serokell.Util.Common     (enumerate, indexedSubList)

import qualified RSCoin.Core              as C

import           RSCoin.Explorer.Error    (ExplorerError (..))
import           RSCoin.Explorer.Extended (CoinsMapExtended, HBlockExtended, Timestamp,
                                           TransactionExtended, TransactionExtension (..),
                                           cmeTotal, isTransactionInteresting,
                                           mkCoinsMapExtended, mkHBlockExtended,
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
    } deriving (Show, Eq)

$(deriveSafeCopy 0 'base ''TransactionIndex)

data AddressData = AddressData
    { _adBalance                 :: !CoinsMapExtended
    , _adTransactions            :: ![TransactionIndex]
    , _adInterestingTransactions :: ![TransactionIndex]
    }

mkAddressData :: AddressData
mkAddressData =
    AddressData
    { _adBalance = mkCoinsMapExtended C.zeroCoinsMap
    , _adTransactions = mempty
    , _adInterestingTransactions = mempty
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

-- | Get number of transactions refering to given address and number
-- of interesting transactions (the second one). Result is timestamped
-- with id of ongoing period.
getAddressTxNumber :: C.Address -> Query (C.PeriodId, (Word, Word))
getAddressTxNumber addr =
    addTimestamp $
    (,) <$>
    views (addresses . at addr) (maybe 0 (genericLength . view adTransactions)) <*>
    views
        (addresses . at addr)
        (maybe 0 (genericLength . view adInterestingTransactions))

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

-- | Like getAddressTransactions, but returns only interesting transactions.
getAddressInterestingTransactions :: C.Address
                                  -> (Word, Word)
                                  -> Query (C.PeriodId, [(Word, TransactionExtended)])
getAddressInterestingTransactions addr indices =
    addTimestamp $
    indexedSubList indices . catMaybes <$>
    (mapM txIdxToTxExtended =<<
     views (addresses . at addr) (maybe [] (view adInterestingTransactions)))

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
    (indexedSubList range . catMaybes) <$>
    (mapM txIdxToTxExtended =<< getRecentTxsIndices False (snd range))

-- | Like getTxsGlobal, but considers only interesting transactions.
getInterestingTxsGlobal :: (Word, Word)
                        -> Query (C.PeriodId, [(Word, TransactionExtended)])
getInterestingTxsGlobal range =
    addTimestamp $
    (indexedSubList range . catMaybes) <$>
    (mapM txIdxToTxExtended =<< getRecentTxsIndices True (snd range))

getRecentTxsIndices :: Bool -> Word -> Query [TransactionIndex]
getRecentTxsIndices onlyInteresting n = do
    blocksTxs <- views hBlocks (V.toList . fmap (C.hbTransactions . C.wmValue))
    foldM step [] . zip [0 .. genericLength blocksTxs - 1] $ blocksTxs
  where
    step
        :: [TransactionIndex]
        -> (C.PeriodId, [C.Transaction])
        -> Query [TransactionIndex]
    step res (blkIdx, blkTxs)
        | length blkTxs == 0 || length res >= fromIntegral n = pure res
        | otherwise = do
            let filterF
                    | onlyInteresting = const $ pure True
                    | otherwise = isTransactionInteresting getTx . snd
            filteredIndices :: [Word] <-
                map fst <$> filterM filterF (zip [0 ..] blkTxs)
            return $ res `mappend` fmap (TransactionIndex blkIdx) filteredIndices

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
    mapM_ applyTxToAddresses $ zip (map (TransactionIndex pId) [0 ..]) extendedTxs

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
applyTxToAddresses (i, C.WithMetadata tx@C.Transaction {..} TransactionExtension {..}) = do
    isInterestingTx <- isTransactionInteresting (readerToState . getTx) tx
    mapM_ (applyTxInput i isInterestingTx) $ zip txInputs teInputAddresses
    mapM_ (applyTxOutput i isInterestingTx) txOutputs

applyTxInput :: TransactionIndex -> Bool -> (C.AddrId, Maybe C.Address) -> Update ()
applyTxInput i isInteresting ((_, _, c), mAddr) =
    whenJust mAddr $ changeAddressData i isInteresting (-c)

applyTxOutput :: TransactionIndex -> Bool -> (C.Address, C.Coin) -> Update ()
applyTxOutput i isInteresting (addr, c) = changeAddressData i isInteresting c addr

changeAddressData :: TransactionIndex -> Bool -> C.Coin -> C.Address -> Update ()
changeAddressData tx isInteresting c addr = do
    ensureAddressExists addr
    Any prepended <-
        addresses . at addr . _Just . adTransactions %%= prependChecked tx
    when (prepended && isInteresting) $ addresses . at addr . _Just . adInterestingTransactions %=
        (tx :)
    let aBalance = addresses . at addr . _Just . adBalance
    aBalance . wmVal %= I.insertWith (+) (C.getColor $ C.coinColor c) c
    aBalance . wmExtension . cmeTotal += C.coinAmount c
  where
    prependChecked i l
        | i `elem` l = (Any False, l)
        | otherwise = (Any True, i : l)

ensureAddressExists :: C.Address -> Update ()
ensureAddressExists addr =
    addresses %= HM.alter (Just . fromMaybe mkAddressData) addr
