-- | Functions related to Coin datatype

module RSCoin.Core.Coin
       ( onColor
       , groupCoinsList
       , coinsMapConsistent
       , mergeCoinsMap
       ) where

import           Control.Exception      (assert)
import           Data.List              (groupBy, sortBy)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust)
import           Data.Ord               (comparing)

import           RSCoin.Core.Primitives (Coin (..), Color)

onColor :: Coin -> Coin -> Ordering
onColor = comparing getColor

-- | Given a list of arbitrary coins, it sums the coins with the same
-- color and returns list of coins of distinct color sorted by the
-- color
groupCoinsList :: [Coin] -> [Coin]
groupCoinsList coins = sortBy onColor $ map sum $ groupBy sameColor coins
  where
    sameColor :: Coin -> Coin -> Bool
    sameColor a b = EQ == onColor a b

-- | Checks a consistency of map from color to coin
coinsMapConsistent :: M.Map Color Coin -> Bool
coinsMapConsistent coins = all keyValid $ M.keys coins
  where
    keyValid k = let coin = fromJust $ M.lookup k coins
                 in getColor coin == k

-- | Given a empty list of coin maps (map
mergeCoinsMap :: [M.Map Color Coin] -> M.Map Color Coin
mergeCoinsMap [] = M.empty
mergeCoinsMap coinMaps =
    assert (all coinsMapConsistent coinMaps) $
    foldr1 (M.unionWith (+)) coinMaps
