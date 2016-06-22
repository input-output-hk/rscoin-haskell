{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Arbitrary instances for Core types.

module Test.RSCoin.Core.Arbitrary
       (
       ) where

import           Test.QuickCheck (Arbitrary (arbitrary), NonNegative (..), NonEmptyList (..), vector, Gen)

import qualified RSCoin.Core     as C
import qualified Data.Map.Strict as M (Map, (!), elems, insert)

instance Arbitrary C.Coin where
    arbitrary = do col <- arbitrary
                   NonNegative coin <- arbitrary
                   return $ C.Coin col coin

instance Arbitrary C.Mintette where
    arbitrary = C.Mintette <$> arbitrary <*> arbitrary

instance Arbitrary C.Hash where
    arbitrary = (C.hash :: C.Mintette -> C.Hash) <$> arbitrary

instance Arbitrary C.Address where
    arbitrary = C.Address <$> arbitrary

