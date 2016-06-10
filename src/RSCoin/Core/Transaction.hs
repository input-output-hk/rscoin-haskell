{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions related to Transaction

module RSCoin.Core.Transaction
       ( validateSum
       , validateSignature
       , getAmountByAddress
       , getAddrIdByAddress
       , chooseAddresses
       , computeOutputAddrids
       ) where

import           Control.Exception      (assert)
import           Data.List              (sortBy, groupBy)
import           Data.Function          (on)
import           Data.Ord               (comparing)
import           Data.Tuple.Select      (sel3)

import           RSCoin.Core.Crypto     (Signature, hash, verify)
import           RSCoin.Core.Primitives (AddrId, Address (..), Coin,
                                         Transaction (..))

instance Ord Transaction where
    compare = comparing hash

-- | Validates that sum of inputs for each color isn't greater than sum of outputs.
validateSum :: Transaction -> Bool
validateSum Transaction{..} =
    let inputSums = map sum $ groupBy ((==) `on` getColor) $ sortBy (compare `on` getColor) $ map sel3 txInputs
        outputSums = map sum $ groupBy ((==) `on` getColor) $ sortBy (compare `on` getColor) $ map snd txOutputs in
    and $ zipWith ((>=) `on` getCoin) inputSums outputSums

-- | Validates that signature is issued by public key associated with given
-- address for the transaction.
validateSignature :: Signature -> Address -> Transaction -> Bool
validateSignature signature (Address pk) = verify pk signature

-- | Given address and transaction returns total amount of money
-- transaction transfers to address.
getAmountByAddress :: Address -> Transaction -> Coin
getAmountByAddress addr Transaction{..} =
    sum $ map snd $ filter ((==) addr . fst) txOutputs

-- | Given address a and transaction returns all addrids that have
-- address equal to a.
getAddrIdByAddress :: Address -> Transaction -> [AddrId]
getAddrIdByAddress addr transaction@Transaction{..} =
    let h = hash transaction in
    map (\(i,(_,c)) -> (h,i,c)) $
        filter ((==) addr . fst . snd) $ [(0 :: Int)..] `zip` txOutputs

-- | Computes optimal (?) usage of addrids to pay the given amount of
-- coins from address. Sum of coins of those addrids should be greater
-- or equal to given value. Here 'optimal' stands for 'trying to
-- include as many addrids as possible', so that means function takes
-- addrids with smaller amount of money first.
chooseAddresses :: [AddrId] -> Coin -> ([AddrId], Coin)
chooseAddresses addrids value =
    chooseOptimal addrids sel3 value

chooseOptimal :: [a] -> (a -> Coin) -> Coin -> ([a], Coin)
chooseOptimal addrids getC value =
    assert (sum (map getC addrids) >= value) $
    let (_,chosenAIds,Just whatsLeft) =
            foldl foldFoo (0, [], Nothing) $ sortBy (comparing getC) addrids
        foldFoo o@(_,_,Just _) _ = o
        foldFoo (accum,values,Nothing) e =
            let val = getC e
                newAccum = accum + val
                newValues = e : values
            in ( newAccum
               , newValues
               , if newAccum >= value
                     then Just $ newAccum - value
                     else Nothing)
    in (chosenAIds, whatsLeft)

-- | This function creates for every address ∈ S_{out} a pair
-- (addr,addrid), where addrid is exactly a usage of this address in
-- this transasction
computeOutputAddrids :: Transaction -> [(AddrId, Address)]
computeOutputAddrids tx@Transaction{..} =
    let h = hash tx in
    map (\((addr, coin), i) -> ((h, i, coin), addr)) $ txOutputs `zip` [0..]
