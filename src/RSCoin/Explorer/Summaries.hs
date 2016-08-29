-- | Summaries for core types.

module RSCoin.Explorer.Summaries
       ( ExtendedAddrId
       , TransactionSummary (..)
       , txSummaryToTx
       , CoinsMapSummary (..)
       , mkCoinsMapSummary
       ) where

import qualified RSCoin.Core as C

import           Data.IntMap (elems)

-- | This is extended version of CoinsMap from RSCoin.Core
data CoinsMapSummary = CoinsMapSummary
    { _cmsCoinsMap   :: C.CoinsMap
    , _cmsCoinAmount :: C.CoinAmount
    } deriving (Show)

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
    } deriving (Show)

txSummaryToTx :: TransactionSummary -> C.Transaction
txSummaryToTx = undefined
