{-# LANGUAGE ViewPatterns #-}

-- | RSCoin.Core.Coin specification

module Test.RSCoin.Core.CoinSpec
       ( spec
       ) where

import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (NonEmptyList (..))

import qualified RSCoin.Core                as C

import           Test.RSCoin.Core.Arbitrary ()

spec :: Spec
spec =
    describe "Coin" $ do
        describe "sameColor" $ do
            prop "returns true iff coins have same color" sameColorCorrect
        describe "sumCoin" $ do
            prop description_sumCoinReturnsSum sumCoinReturnsSum
  where
    description_sumCoinReturnsSum = "given non-empty list of coins with " ++
      "the same color, returns coin with the same color and value equal to " ++
      "sum of values"

sameColorCorrect :: C.Coin -> C.Coin -> Bool
sameColorCorrect c1 c2 = (C.getColor c1 == C.getColor c2) == C.sameColor c1 c2

sumCoinReturnsSum :: C.Color -> NonEmptyList Rational -> Bool
sumCoinReturnsSum color (getNonEmpty -> values) =
    let coins = map (C.Coin color) values
        s = C.sumCoin coins
        expected = C.Coin color $ sum values
    in s == expected
