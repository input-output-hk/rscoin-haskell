{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Summaries for core types.

module RSCoin.Explorer.Summaries
       ( ExtendedAddrId

       , TransactionSummary (..)
       , txSummaryToTx

       , CoinsMapSummary
       , mkCoinsMapSummary
       , cmsCoinsMap
       , cmsCoinAmount
       ) where

import           Control.Lens  (makeLenses)
import           Data.IntMap   (elems)
import           Data.SafeCopy (base, deriveSafeCopy)
import           GHC.Generics  (Generic)

import qualified RSCoin.Core   as C

-- | This is extended version of CoinsMap from RSCoin.Core
data CoinsMapSummary = CoinsMapSummary
    { _cmsCoinsMap   :: C.CoinsMap
    , _cmsCoinAmount :: C.CoinAmount
    } deriving (Show, Generic)

$(makeLenses ''CoinsMapSummary)

mkCoinsMapSummary :: C.CoinsMap -> CoinsMapSummary
mkCoinsMapSummary coins = CoinsMapSummary coins  . sum . map C.getCoin $ elems coins

-- | This is extended version of AddrId from RSCoin.Core
type ExtendedAddrId = (C.TransactionId, Int, C.Coin, Maybe C.Address)

-- | This is extended version of Transaction from RSCoin.Core
data TransactionSummary = TransactionSummary
    { txsId         :: C.TransactionId
    , txsInputs     :: [ExtendedAddrId]
    , txsOutputs    :: [(C.Address, C.Coin)]
    , txsInputsSum  :: CoinsMapSummary
    , txsOutputsSum :: CoinsMapSummary
    } deriving (Show, Generic)

txSummaryToTx :: TransactionSummary -> C.Transaction
txSummaryToTx = undefined

$(deriveSafeCopy 0 'base ''TransactionSummary)
$(deriveSafeCopy 0 'base ''CoinsMapSummary)
