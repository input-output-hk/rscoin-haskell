{-# LANGUAGE FlexibleContexts #-}

-- | Functions related to Coin datatype

module RSCoin.Core.Coin
       ( onColor
       , onCoin
       , isPositiveCoin
       , isZeroCoin
       , isNegativeCoin
       , sameColor
       , sumCoin
       , groupCoinsList
       , CoinsMap
       , zeroCoinsMap
       , coinsToList
       , coinsToMap
       , coinsMapConsistent
       , mergeCoinsMaps
       , coinsMapsEqual
       , addCoinsMap
       , subtractCoinsMap
       ) where

import           Control.Exception         (assert)
import           Control.Lens              (at, (%~), (&), _Just)
import           Data.Foldable             (foldr')
import qualified Data.IntMap.Strict        as M
import           Data.List                 (groupBy, sortBy)
import           Data.Maybe                (fromJust)
import           Data.Ord                  (comparing)

import           RSCoin.Core.Primitives (Coin (..), Color (..))

onColor :: Coin -> Coin -> Ordering
onColor = comparing getColor

onCoin :: Coin -> Coin -> Ordering
onCoin = comparing getCoin

isPositiveCoin :: Coin -> Bool
isPositiveCoin = (> 0) . getCoin

isZeroCoin :: Coin -> Bool
isZeroCoin = (== 0) . getCoin

isNegativeCoin :: Coin -> Bool
isNegativeCoin = (< 0) . getCoin

sameColor :: Coin -> Coin -> Bool
sameColor a b = EQ == onColor a b

sumCoin :: [Coin] -> Coin
sumCoin [] = error "sumCoin called with empty coin list"
sumCoin xs@(c:_) = foldr' (+) c {getCoin = 0} xs

-- | Given a list of arbitrary coins, it sums the coins with the same
-- color and returns list of coins of distinct color sorted by the
-- color. Also deletes negative coins.
groupCoinsList :: [Coin] -> [Coin]
groupCoinsList coins =
    map sumCoin $
    filter (not . null) $
    groupBy sameColor $ sortBy onColor $ filter isPositiveCoin coins

type CoinsMap = M.IntMap Coin

-- | CoinsMap representing absence of coins.
zeroCoinsMap :: CoinsMap
zeroCoinsMap = M.empty

-- | Translates a map of coins to the list, sorted by color
coinsToList :: CoinsMap -> [Coin]
coinsToList coinsMap = groupCoinsList $ M.elems coinsMap

-- | Translates a list of coins to the map
coinsToMap :: [Coin] -> CoinsMap
coinsToMap = M.fromList . map (\c -> (getC . getColor $ c, c)) . groupCoinsList

-- | Checks a consistency of map from color to coin
coinsMapConsistent :: CoinsMap -> Bool
coinsMapConsistent coins = all keyValid $ M.keys coins
  where
    keyValid k = let coin = fromJust $ M.lookup k coins
                 in (getC $ getColor coin) == k

-- | Given a empty list of coin maps (map
mergeCoinsMaps :: [CoinsMap] -> CoinsMap
mergeCoinsMaps [] = M.empty
mergeCoinsMaps coinMaps =
    assert (all coinsMapConsistent coinMaps) $
    foldr1 (M.unionWith (+)) coinMaps

coinsMapsEqual :: CoinsMap -> CoinsMap -> Bool
coinsMapsEqual m1 m2 = dropZeros m1 == dropZeros m2
  where
    dropZeros = M.filter ((== 0) . getCoin)

-- | For each color in the first map, if there exists this color in
-- the second map, then value in the first map is increased by
-- corresponding value from the second map.
addCoinsMap :: CoinsMap -> CoinsMap -> CoinsMap
addCoinsMap = coinsMapOperation (+)

-- | For each color in the first map, if there exists this color in
-- the second map, then value in the first map is decreased by
-- corresponding value from the second map.
subtractCoinsMap :: CoinsMap -> CoinsMap -> CoinsMap
subtractCoinsMap = coinsMapOperation (-)

coinsMapOperation :: (Coin -> Coin -> Coin) -> CoinsMap -> CoinsMap -> CoinsMap
coinsMapOperation op operand1 = M.foldlWithKey' step operand1
  where
    step accum color coins =
        if color `M.member` accum
            then accum & at color . _Just %~ (`op` coins)
            else accum
