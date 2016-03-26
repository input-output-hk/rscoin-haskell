-- | Functions related to Transaction

module RSCoin.Core.Transaction
       ( validateSum
       , getAmountByAddress
       ) where

import           RSCoin.Core.Primitives (Address, Coin, Transaction (..))

validateSum :: Transaction -> Bool
validateSum Transaction{..} =
    let inputSum = sum $ map (\(_,_,c) -> c) txInputs
        outputSum = sum $ map snd txOutputs in
    inputSum >= outputSum

getAmountByAddress :: Address -> Transaction -> Coin
getAmountByAddress addr Transaction{..} =
    sum $ map snd $ filter ((==) addr . fst) txOutputs
