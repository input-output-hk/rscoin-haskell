{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Functions related to Transaction

module RSCoin.Core.Transaction
       ( validateSum
       , validateSignature
       , getAmountByAddress
       , getAddrIdByAddress
       , chooseAddresses
       , computeOutputAddrids
       ) where

import           Control.Arrow          ((&&&))
import           Control.Exception      (assert)
import           Data.Function          (on)
import           Data.List              (delete, groupBy, nub, sortBy)
import qualified Data.Map               as M
import           Data.Ord               (comparing)
import           Data.Tuple.Select      (sel3)

import           RSCoin.Core.Coin       (coinsToMap)
import           RSCoin.Core.Crypto     (Signature, hash, verify)
import           RSCoin.Core.Primitives (AddrId, Address (..), Coin (..), Color,
                                         Transaction (..), grey)

instance Ord Transaction where
    compare = comparing hash

-- | Validates that sum of inputs for each color isn't greater than
-- sum of outputs, and what's left can be painted by grey coins.
validateSum :: Transaction -> Bool
validateSum Transaction{..} =
    and [ totalInputs >= totalOutputs
        , greyInputs >= greyOutputs + totalUnpaintedSum ]
  where
    inputs  = coinsToMap $ map sel3 txInputs
    outputs = coinsToMap $ map snd txOutputs
    totalInputs  = sum $ map getCoin $ M.elems inputs
    totalOutputs = sum $ map getCoin $ M.elems outputs
    greyInputs  = getCoin $ M.findWithDefault 0 grey inputs
    greyOutputs = getCoin $ M.findWithDefault 0 grey outputs
    txColors = delete grey $ nub $ (M.keys inputs ++ M.keys outputs)
    foldfoo0 color unp =
        let zero = Coin color 0
            outputOfThisColor = M.findWithDefault zero color outputs
            inputOfThisColor = M.findWithDefault zero color inputs
        in if outputOfThisColor <= inputOfThisColor
           then unp
           else M.insert color (outputOfThisColor - inputOfThisColor) unp
    unpainted = foldr foldfoo0 M.empty txColors
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

-- | For each color, computes optimal usage of addrids to pay the given amount of
-- coins. Sum of coins of those addrids should be greater
-- or equal to given value, for each color. Here 'optimal' stands for 'trying to
-- include as many addrids as possible', so that means function takes
-- addrids with smaller amount of money first.
chooseAddresses :: [AddrId] -> M.Map Color Coin -> Maybe (M.Map Color ([AddrId], Coin))
chooseAddresses addrids valueMap =
    chooseOptimal addrids' sel3 valueMap'
    where addrids' = filter ((/=0) . getCoin . sel3) addrids
          valueMap' = M.filter ((/=0) . getCoin) valueMap

chooseOptimal
    :: forall a.
       [a]                             -- ^ Elements we're choosing from
    -> (a -> Coin)                     -- ^ Getter of coins from the element
    -> M.Map Color Coin                -- ^ Map with amount of coins for each color
    -> Maybe (M.Map Color ([a], Coin)) -- ^ Map with chosen elements and change for each color
                                       -- If nothing, value can't be chosen (no money)
chooseOptimal addrids getC valueMap =
    -- In case there are less colors in addrList than in valueList
    -- filler coins are added to short-circuit the comparison of lists.
    assert
        (map (sum . map getC) addrList ++ repeat (Coin 0 0) >= M.elems valueMap) $
    M.fromList <$> mapM
        (\(color, value) ->
              (color,) <$> chooseHelper (M.findWithDefault [] color addrMap) value)
        (M.toList valueMap)
  where
    -- List of lists of addrids. Each sublist has the same color
    -- and the extern list is sorted by it. Inner list of the same
    -- color is sorted by coins amount.
    addrList :: [[a]]
    addrList =
        groupBy ((==) `on` (getColor . getC)) $
        sortBy (comparing (getColor . getC)) $
        sortBy (comparing (getCoin . getC)) addrids
    -- addrMap :: M.Map Color [a]
    -- Map from each color to addrids with a coin of that color
    addrMap = M.fromList $ map ((getColor . getC . head) &&& id) addrList
    -- chooseHelper :: [a] -> Coin -> ([a], Coin)
    -- This function goes through a list of addrids and calculates the optimal
    chooseHelper list value =
        -- choice of addrids and the coins that are left
        let foldFoo o@(_,_,Just _) _ = o
            foldFoo (accum,values,Nothing) e =
                let val = getC e
                    newAccum = accum + val
                    newValues = e : values
                in ( newAccum
                   , newValues
                   , if newAccum >= value
                     then Just $ newAccum - value
                     else Nothing)
        in case foldl foldFoo (Coin (getColor value) 0, [], Nothing) list of
                    (_,chosenAIds,Just whatsLeft) -> Just (chosenAIds, whatsLeft)
                    (_,_,Nothing) -> Nothing

-- | This function creates for every address ∈ S_{out} a pair
-- (addr,addrid), where addrid is exactly a usage of this address in
-- this transasction
computeOutputAddrids :: Transaction -> [(AddrId, Address)]
computeOutputAddrids tx@Transaction{..} =
    let h = hash tx in
    map (\((addr, coin), i) -> ((h, i, coin), addr)) $ txOutputs `zip` [0..]
