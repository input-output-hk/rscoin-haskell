-- | TransactionSummary type.

module RSCoin.Explorer.TransactionSummary
       ( ExtendedAddrId
       , TransactionSummary (..)
       , txSummaryToTx
       ) where

import qualified RSCoin.Core as C

-- | This is extended version of AddrId from RSCoin.Core
type ExtendedAddrId = (C.TransactionId, Int, C.Coin, Maybe C.Address)

-- | This is extended version of Transaction from RSCoin.Core
data TransactionSummary = TransactionSummary
    { txsId           :: C.TransactionId
    , txsInputs       :: [ExtendedAddrId]
    , txsOutputs      :: [(C.Address, C.Coin)]
    , txsInputsSum    :: C.CoinsMap
    , txsInputsTotal  :: C.CoinAmount
    , txsOutputsSum   :: C.CoinsMap
    , txsOutputsTotal :: C.CoinAmount
    } deriving (Show)

txSummaryToTx :: TransactionSummary -> C.Transaction
txSummaryToTx = undefined
