{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions related to Transaction

module RSCoin.Core.Transaction
       ( validateSum
       , validateSignature
       , getAmountByAddress
       , getAddrIdByAddress
       , chooseAddresses
       , chooseAddresses'
       , computeOutputAddrids
       ) where

import           Control.Arrow          ((&&&))
import           Control.Exception      (assert)
import           Data.Function          (on)
import           Data.List              (delete, groupBy, sortBy)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust)
import           Data.Ord               (comparing)
import           Data.Tuple.Select      (sel3)

import           RSCoin.Core.Coin       (coinsToMap, groupCoinsList)
import           RSCoin.Core.Crypto     (Signature, hash, verify)
import           RSCoin.Core.Primitives (AddrId, Address (..), Coin (..), Color,
                                         Transaction (..))

instance Ord Transaction where
    compare = comparing hash

-- | Validates that sum of inputs for each color isn't greater than
-- sum of outputs, and what's left can be painted by grey coins.
validateSum :: Transaction -> Bool
validateSum Transaction{..} =
    greyInputs >= greyOutputs + totalUnpaintedSum
  where
    inputs = coinsToMap $ groupCoinsList $ map sel3 txInputs
    outputs = coinsToMap $ groupCoinsList $ map snd txOutputs
    greyInputs = getCoin $ M.findWithDefault 0 0 inputs
    greyOutputs = getCoin $ M.findWithDefault 0 0 outputs
    inputColors = delete 0 $ M.keys inputs
    foldfoo0 color unp =
        let outputOfThisColor = M.findWithDefault 0 color outputs
            inputOfThisColor = fromJust $ M.lookup color inputs
        in if outputOfThisColor <= inputOfThisColor
           then unp
           else M.insert color (outputOfThisColor - inputOfThisColor) unp
    unpainted = foldr foldfoo0 M.empty inputColors
    totalUnpaintedSum = sum $ map getCoin $ M.elems unpainted

-- | Validates that signature is issued by public key associated with given
-- address for the transaction.
validateSignature :: Signature -> Address -> Transaction -> Bool
validateSignature signature (Address pk) = verify pk signature

-- | Given address and transaction returns total amount of money
-- transaction transfers to address.
getAmountByAddress :: Address -> Transaction -> M.Map Color Coin
getAmountByAddress addr Transaction{..} =
    let pair c = (getColor c, c) in
    M.fromListWith (+) $ map (pair . snd) $ filter ((==) addr . fst) txOutputs

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

chooseAddresses' :: [AddrId] -> M.Map Int Rational -> M.Map Int ([AddrId], Rational)
chooseAddresses' addrids =
    chooseOptimal' addrids sel3

chooseOptimal' :: [a] -> (a -> Coin) -> M.Map Int Rational -> M.Map Int ([a], Rational)
chooseOptimal' addrids getC valueMap =
    assert (map (sum . map getC) addrList ++ repeat (Coin 0 0) >= valueList) $
    M.mapWithKey
        (\color value ->
              chooseHelper (addrMap M.! color) (Coin color value))
        valueMap
  where
    addrList =
        groupBy ((==) `on` (getColor . getC)) $
        sortBy (comparing (getCoin . getC)) $
        sortBy (comparing (getColor . getC)) addrids
    valueList = map (uncurry Coin) $ M.toList valueMap
    chooseHelper list value =
        let (_,chosenAIds,Just whatsLeft) = foldl foldFoo (0, [], Nothing) list
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
        in (chosenAIds, getCoin whatsLeft)
    addrMap = M.fromList $ map ((getColor . getC . head) &&& id) addrList

-- | This function creates for every address ∈ S_{out} a pair
-- (addr,addrid), where addrid is exactly a usage of this address in
-- this transasction
computeOutputAddrids :: Transaction -> [(AddrId, Address)]
computeOutputAddrids tx@Transaction{..} =
    let h = hash tx in
    map (\((addr, coin), i) -> ((h, i, coin), addr)) $ txOutputs `zip` [0..]
