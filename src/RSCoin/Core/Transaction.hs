{-# LANGUAGE ScopedTypeVariables #-}
-- | Functions related to Transaction

module RSCoin.Core.Transaction
       ( validateSum
       , getAmountByAddress
       , getAddrIdByAddress
       ) where

import           RSCoin.Core.Crypto     (hash)
import           RSCoin.Core.Primitives (AddrId, Address, Coin,
                                         Transaction (..))

validateSum :: Transaction -> Bool
validateSum Transaction{..} =
    let inputSum = sum $ map (\(_,_,c) -> c) txInputs
        outputSum = sum $ map snd txOutputs in
    inputSum >= outputSum

getAmountByAddress :: Address -> Transaction -> Coin
getAmountByAddress addr Transaction{..} =
    sum $ map snd $ filter ((==) addr . fst) txOutputs

getAddrIdByAddress :: Address -> Transaction -> [AddrId]
getAddrIdByAddress addr transaction@Transaction{..} =
    let h = hash transaction in
    map (\(i,(_,c)) -> (h,i,c)) $
        filter ((==) addr . fst . snd) $ [(0 :: Int)..] `zip` txOutputs
