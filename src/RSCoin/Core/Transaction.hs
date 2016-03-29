{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions related to Transaction

module RSCoin.Core.Transaction
       ( validateSum
       , validateSignature
       , getAmountByAddress
       , getAddrIdByAddress
       , chooseAddresses
       ) where

import           Control.Exception      (assert)
import           Data.List              (sortBy)
import           Data.Monoid            ((<>))
import           Data.Ord               (comparing)
import           Data.Tuple.Select      (sel3)

import           RSCoin.Core.Crypto     (Signature, hash, verify)
import           RSCoin.Core.Primitives (AddrId, Address (..), Coin,
                                         Transaction (..))

-- I dunno if we need it, maybe it will be useful at some point (I've
-- got some feeling it will).
instance Monoid Transaction where
    mempty = Transaction [] []
    a `mappend` b =
        Transaction (txInputs a <> txInputs b) (txOutputs a <> txOutputs b)

-- | Validates that sum of inputs isn't greater than sum of outputs.
validateSum :: Transaction -> Bool
validateSum Transaction{..} =
    let inputSum = sum $ map sel3 txInputs
        outputSum = sum $ map snd txOutputs in
    inputSum >= outputSum

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
    assert (sum (map sel3 addrids) >= value) $
    let (_,chosenAIds,Just whatsLeft) =
            foldl foldFoo (0, [], Nothing) $ sortBy (comparing sel3) addrids
        foldFoo o@(_,_,Just _) _ = o
        foldFoo (accum,values,Nothing) addrid@(_,_,aval) =
            let newAccum = accum + aval
                newValues = addrid : values
            in ( newAccum
               , newValues
               , if newAccum >= value
                     then Just $ newAccum - value
                     else Nothing)
    in (chosenAIds, whatsLeft)
