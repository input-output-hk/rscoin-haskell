{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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

       , TransactionExtension (..)
       , TransactionExtended
       , mkTransactionExtension

       , HBlockExtension (..)
       , HBlockExtended
       , mkHBlockExtension
       , mkHBlockExtended
       ) where

import           Control.Lens          (makeLenses, view, _2, _3)
import           Data.Bifunctor        (second)
import           Data.IntMap           (elems)
import           Data.List             (genericLength)
import qualified Data.Map              as M (elems, fromListWith,
                                             update)
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
    { teId             :: !C.TransactionId
    , tePeriodId       :: !C.PeriodId
    , teInputAddresses :: ![Maybe C.Address]
    , teInputsSum      :: !CoinsMapExtended
    , teOutputsSum     :: !CoinsMapExtended
    , teTimestamp      :: !Timestamp
    } deriving (Show, Generic)

type TransactionExtended = C.WithMetadata C.Transaction TransactionExtension

mkTransactionExtension
    :: forall m.
       Monad m
    => C.PeriodId
    -> Timestamp
    -> (C.TransactionId -> m (Maybe C.Transaction))
    -> C.Transaction
    -> m TransactionExtension
mkTransactionExtension pId timestamp getTx tx@C.Transaction {..} =
    TransactionExtension (C.hash tx) pId <$> mapM getAddress txInputs <*>
    pure inputsSum <*>
    pure outputsSum <*>
    pure timestamp
  where
    getAddress :: C.AddrId -> m (Maybe C.Address)
    getAddress (txId, idx, _) = do
        (fmap fst . (`atMay` idx) . C.txOutputs =<<) <$> getTx txId
    inputsSum = mkCoinsMapExtended . C.coinsToMap . map (view _3) $ txInputs
    outputsSum = mkCoinsMapExtended . C.coinsToMap . map (view _2) $ txOutputs

-- | Extension of HBlock.
data HBlockExtension = HBlockExtension
    { hbeHeight    :: !C.PeriodId
    , hbeTimestamp :: !Timestamp
    , hbeTxNumber  :: !Word
    , hbeTotalSent :: !C.CoinAmount
    -- , hbeSize      :: !Byte
    } deriving (Show, Generic)

mkHBlockExtension :: C.PeriodId
                  -> C.WithMetadata C.HBlock C.HBlockMetadata
                  -> HBlockExtension
mkHBlockExtension pId C.WithMetadata {wmValue = C.HBlock {..}
                                     ,wmMetadata = C.HBlockMetadata {..}} =
    HBlockExtension
    { hbeHeight = pId
    , hbeTimestamp = hbmTimestamp
    , hbeTxNumber = genericLength hbTransactions
    , hbeTotalSent = totalSent
    -- , hbeSize = size
    }
  where
    totalSent = sum . map transactionTotalSent $ hbTransactions
    transactionTotalSent :: C.Transaction -> C.CoinAmount
    transactionTotalSent C.Transaction{..} =
        let amountMap =
                M.fromListWith (+) $
                map (\(_, i, C.coinAmount -> c) -> (fst $ txOutputs !! i, c)) $
                txInputs
            step (adr, c) cMap = M.update (\oldC -> Just $ oldC - c) adr cMap
            newMap =
                foldr step amountMap (map (\(a, C.coinAmount -> c) ->
                                               (a, c)) txOutputs)
        in sum $ M.elems newMap
    -- size = undefined

type HBlockExtended = C.WithMetadata C.HBlock HBlockExtension

mkHBlockExtended :: C.PeriodId
                 -> C.WithMetadata C.HBlock C.HBlockMetadata
                 -> HBlockExtended
mkHBlockExtended pId blkWithMeta =
    second (const $ mkHBlockExtension pId blkWithMeta) blkWithMeta

$(deriveSafeCopy 0 'base ''CoinsMapExtension)
$(deriveSafeCopy 0 'base ''TransactionExtension)
$(deriveSafeCopy 0 'base ''HBlockExtension)
