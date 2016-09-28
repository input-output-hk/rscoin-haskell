{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Extensions of some Core types. They contain extra data needed by
-- web clients.

module RSCoin.Explorer.Extended
       (
         CoinsMapExtended
       , CoinsMapExtension
       , cmeTotal
       , mkCoinsMapExtended

       , Timestamp

       , isTransactionInteresting

       , TransactionExtension (..)
       , TransactionExtended
       , mkTransactionExtension

       , HBlockExtension (..)
       , HBlockExtended
       , mkHBlockExtension
       , mkHBlockExtended
       ) where

import           Control.Lens          (makeLenses, view, (^.), _2, _3)
import           Control.Monad         (filterM)
import           Data.Bifunctor        (second)
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS
import           Data.IntMap           (elems)
import           Data.List             (genericLength)
import qualified Data.Map              as M (elems, fromListWith, update)
import           Data.Maybe            (catMaybes)
import           Data.SafeCopy         (base, deriveSafeCopy)
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics          (Generic)
import           Safe                  (atMay)

-- import           Serokell.Data.Memory.Units (Byte)

import qualified RSCoin.Core           as C

-- | Extension of CoinsMap.
data CoinsMapExtension = CoinsMapExtension
    { _cmeTotal :: !C.CoinAmount  -- ^ Total number of coins (regardless of color)
    } deriving (Show, Generic)

$(makeLenses ''CoinsMapExtension)

type CoinsMapExtended = C.WithMetadata C.CoinsMap CoinsMapExtension

mkCoinsMapExtended :: C.CoinsMap -> CoinsMapExtended
mkCoinsMapExtended coins =
    C.WithMetadata coins . CoinsMapExtension . sum . map C.coinAmount . elems $
    coins

type Timestamp = POSIXTime

-- | Extension of Transaction.
data TransactionExtension = TransactionExtension
    { teId               :: !C.TransactionId
    , tePeriodId         :: !C.PeriodId
    , teInputAddresses   :: ![Maybe C.Address]
    , teInputsSum        :: !CoinsMapExtended
    -- List is used to simplify JSON encoding
    , teSumPerInputAddr  :: ![(C.Address, C.CoinAmount)]
    , teOutputsSum       :: !CoinsMapExtended
    , teSumPerOutputAddr :: ![(C.Address, C.CoinAmount)]
    , teTimestamp        :: !Timestamp
    } deriving (Show, Generic)

type TransactionExtended = C.WithMetadata C.Transaction TransactionExtension

getAddress
    :: forall m.
       Monad m
    => (C.TransactionId -> m (Maybe C.Transaction))
    -> C.AddrId
    -> m (Maybe C.Address)
getAddress getTx (txId, idx, _) =
    (fmap fst . (`atMay` idx) . C.txOutputs =<<) <$> getTx txId

getAddrAmountPair
    :: forall m.
       Monad m
    => (C.TransactionId -> m (Maybe C.Transaction))
    -> C.AddrId
    -> m (Maybe (C.Address, C.CoinAmount))
getAddrAmountPair getTx addrId =
    fmap (, C.coinAmount (addrId ^. _3)) <$> getAddress getTx addrId

mkTransactionExtension
    :: forall m.
       Monad m
    => C.PeriodId
    -> Timestamp
    -> (C.TransactionId -> m (Maybe C.Transaction))
    -> C.Transaction
    -> m TransactionExtension
mkTransactionExtension pId timestamp getTx tx@C.Transaction {..} =
    TransactionExtension (C.hash tx) pId <$> mapM (getAddress getTx) txInputs <*>
    pure inputsSum <*>
    sumPerInputAddr <*>
    pure outputsSum <*>
    pure sumPerOutputAddr <*>
    pure timestamp
  where
    inputsSum = mkCoinsMapExtended . C.coinsToMap . map (view _3) $ txInputs
    groupByAddr = HM.toList . HM.fromListWith (+) . map (second C.coinAmount)
    sumPerInputAddr =
        groupByAddr . catMaybes <$>
        mapM (\a -> fmap (, view _3 a) <$> getAddress getTx a) txInputs
    outputsSum = mkCoinsMapExtended . C.coinsToMap . map (view _2) $ txOutputs
    sumPerOutputAddr = groupByAddr txOutputs

isTransactionInteresting
    :: forall m.
       Monad m
    => (C.TransactionId -> m (Maybe C.Transaction)) -> C.Transaction -> m Bool
isTransactionInteresting getTx C.Transaction {..} =
    checkSets (HS.fromList . map fst $ txOutputs) . HS.fromList . catMaybes <$>
    mapM (getAddress getTx) txInputs
  where
    checkSets outs ins = ins /= outs || HS.size outs /= 1

-- | Extension of HBlock.
data HBlockExtension = HBlockExtension
    { hbeHeight              :: !C.PeriodId
    , hbeTimestamp           :: !Timestamp
    , hbeTxNumber            :: !Word
    , hbeInterestingTxNumber :: !Word
    , hbeTotalSent           :: !C.CoinAmount
    -- , hbeSize      :: !Byte
    } deriving (Show, Generic)

mkHBlockExtension ::
    forall m.
       Monad m
    => C.PeriodId
    -> C.WithMetadata C.HBlock C.HBlockMetadata
    -> (C.TransactionId -> m (Maybe C.Transaction))
    -> m HBlockExtension
mkHBlockExtension pId C.WithMetadata {wmValue = C.HBlock {..}
                                     ,wmMetadata = C.HBlockMetadata {..}} getTx =
    HBlockExtension pId hbmTimestamp (genericLength hbTransactions) <$>
    (genericLength <$> interestingTxs) <*>
    totalSent
  where
    interestingTxs = filterM (isTransactionInteresting getTx) hbTransactions
    totalSent = sum <$> mapM transactionTotalSent hbTransactions
    transactionTotalSent :: C.Transaction -> m C.CoinAmount
    transactionTotalSent C.Transaction {..} = do
        amountMap <-
            M.fromListWith (+) . catMaybes <$> mapM (getAddrAmountPair getTx) txInputs
        let step (adr, c) cMap = M.update (Just . subtract c) adr cMap
            newMap =
                foldr
                    step
                    amountMap
                    (map (\(a, C.coinAmount -> c) -> (a, c)) txOutputs)
        return $ sum $ M.elems newMap

type HBlockExtended = C.WithMetadata C.HBlock HBlockExtension

mkHBlockExtended ::
    forall m.
    Monad m
    => C.PeriodId
    -> C.WithMetadata C.HBlock C.HBlockMetadata
    -> (C.TransactionId -> m (Maybe C.Transaction))
    -> m HBlockExtended
mkHBlockExtended pId blkWithMeta getTx = do
    newHBExtension <- mkHBlockExtension pId blkWithMeta getTx
    return $ second (const newHBExtension) blkWithMeta

$(deriveSafeCopy 0 'base ''CoinsMapExtension)
$(deriveSafeCopy 0 'base ''TransactionExtension)
$(deriveSafeCopy 0 'base ''HBlockExtension)
