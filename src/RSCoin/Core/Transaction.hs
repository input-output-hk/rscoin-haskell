{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions related to Transaction

module RSCoin.Core.Transaction
       ( validateSum
       , validateSignature
       , getAmountByAddress
       , getCoinsByAddress
       , getAddrIdByAddress
       , chooseAddresses
       , computeOutputAddrids
       ) where

import           Data.Function          (on)
import           Data.List              (groupBy, sortBy)
import           Data.Map.Strict        (Map, fromListWith)
import           Data.Ord               (comparing)
import           Data.Tuple.Select      (sel3)

import           RSCoin.Core.Crypto     (Signature, hash, verify)
import           RSCoin.Core.Primitives (AddrId, Address (..), Coin (..), Color,
                                         Transaction (..))

instance Ord Transaction where
    compare = comparing hash

-- | Validates that sum of inputs for each color isn't greater than sum of outputs.
validateSum :: Transaction -> Bool
validateSum Transaction{..} =
    let inputSums =
            map sum $
            groupBy ((==) `on` getColor) $
            sortBy (comparing getColor) $ map sel3 txInputs
        outputSums =
            map sum $
            groupBy ((==) `on` getColor) $
            sortBy (comparing getColor) $ map snd txOutputs
    in and $ zipWith ((>=) `on` getCoin) inputSums outputSums

-- | Validates that signature is issued by public key associated with given
-- address for the transaction.
validateSignature :: Signature -> Address -> Transaction -> Bool
validateSignature signature (Address pk) = verify pk signature

-- | Given address and transaction returns total amount of money
-- transaction transfers to address.
getAmountByAddress :: Address -> Transaction -> Map Color Rational
getAmountByAddress addr Transaction{..} =
    let pair c = (getColor c, getCoin c) in
    fromListWith (+) $ map (pair . snd) $ filter ((==) addr . fst) txOutputs

-- | Same as getAmountByAddress but returns Coin inside the map
-- TODO rewrite getAmountByAddress to have signature of this function
getCoinsByAddress :: Address -> Transaction -> Map Color Coin
getCoinsByAddress a t = undefined (getAmountByAddress a t)

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
--chooseOptimal :: [a] -> (a -> Coin) -> Map Int Rational -> Map Int ([a], Rational)
chooseOptimal addrids getC valueMap = undefined
    {-let coinList = map sum $
                   groupBy ((==) `on` (getColor . getC)) $
                   sortBy (comparing (getColor . getC)) addrids
        valueList = map (uncurry $ flip Coin) $
                    toList valueMap
        addrList = sortBy (comparing getC) addrids
    in assert (coinList ++ repeat (Coin 0 0) >= valueList) $
      let newMap =
              map (\value -> foldl foldFoo (0, [], Nothing) addrList) valueMap
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
      in (chosenAIds, whatsLeft)-}

-- | This function creates for every address ∈ S_{out} a pair
-- (addr,addrid), where addrid is exactly a usage of this address in
-- this transasction
computeOutputAddrids :: Transaction -> [(AddrId, Address)]
computeOutputAddrids tx@Transaction{..} =
    let h = hash tx in
    map (\((addr, coin), i) -> ((h, i, coin), addr)) $ txOutputs `zip` [0..]
