-- | Functions related to Coin datatype

module Coin (groupCoins) where

import           Data.List              (groupBy, sortBy)
import           Data.Ord               (comparing)

import           RSCoin.Core.Primitives (Coin (..))

onColor :: Coin -> Coin -> Ordering
onColor = comparing getColor

groupCoins :: [Coin] -> [Coin]
groupCoins coins = sortBy onColor $ map sum $ groupBy sameColor coins
  where
    sameColor :: Coin -> Coin -> Bool
    sameColor a b = EQ == onColor a b
