{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Summaries for core types.

module RSCoin.Explorer.Summaries
       ( ExtendedAddrId

       , CoinsMapSummary
       , cmsCoinsMap
       , cmsCoinAmount
       , mkCoinsMapSummary

       , TransactionSummary (..)
       , txSummaryToTx

       , HBlockSummary (..)
       ) where

import           Control.Lens               (makeLenses)
import           Data.IntMap                (elems)
import           Data.SafeCopy              (base, deriveSafeCopy)
import           Data.Time.Clock.POSIX      (POSIXTime)
import           GHC.Generics               (Generic)

import           Serokell.Data.Memory.Units (Byte)

import qualified RSCoin.Core                as C

-- | This is extended version of CoinsMap from RSCoin.Core
data CoinsMapSummary = CoinsMapSummary
    { _cmsCoinsMap   :: C.CoinsMap
    , _cmsCoinAmount :: C.CoinAmount
    } deriving (Show, Generic)

$(makeLenses ''CoinsMapSummary)

mkCoinsMapSummary :: C.CoinsMap -> CoinsMapSummary
mkCoinsMapSummary coins =
    CoinsMapSummary coins  . sum . map C.getCoin . elems $ coins

-- | This is extended version of AddrId from RSCoin.Core
type ExtendedAddrId = (C.TransactionId, Int, C.Coin, Maybe C.Address)

extendedAddrIdToAddrId :: ExtendedAddrId -> C.AddrId
extendedAddrIdToAddrId (txId, idx, c, _) = (txId, idx, c)

-- | This is extended version of Transaction from RSCoin.Core
data TransactionSummary = TransactionSummary
    { txsId         :: C.TransactionId
    , txsInputs     :: [ExtendedAddrId]
    , txsOutputs    :: [(C.Address, C.Coin)]
    , txsInputsSum  :: CoinsMapSummary
    , txsOutputsSum :: CoinsMapSummary
    } deriving (Show, Generic)

txSummaryToTx :: TransactionSummary -> C.Transaction
txSummaryToTx TransactionSummary {..} =
    C.Transaction
    { txInputs = map extendedAddrIdToAddrId txsInputs
    , txOutputs = txsOutputs
    }

data HBlockSummary = HBlockSummary
    { hbsHeight    :: !C.PeriodId
    , hbsTimeStamp :: !POSIXTime
    , hbsTxNumber  :: !Word
    , hbsTotalSent :: !C.CoinAmount
    , hbsSize      :: !Byte
    } deriving (Show, Generic)

$(deriveSafeCopy 0 'base ''CoinsMapSummary)
$(deriveSafeCopy 0 'base ''TransactionSummary)
$(deriveSafeCopy 0 'base ''HBlockSummary)
