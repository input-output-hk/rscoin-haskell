-- | Arbitrary instances for Core types.

module Test.RSCoin.Core.Arbitrary
       (
       ) where

import           Test.QuickCheck (Arbitrary (arbitrary))

import qualified RSCoin.Core     as C

instance Arbitrary C.Coin where
    arbitrary = C.Coin <$> arbitrary <*> arbitrary

instance Arbitrary C.Mintette where
    arbitrary = C.Mintette <$> arbitrary <*> arbitrary

instance Arbitrary C.Hash where
    arbitrary = (C.hash :: C.Mintette -> C.Hash) <$> arbitrary

instance Arbitrary C.Address where
    arbitrary = C.Address <$> arbitrary

instance Arbitrary C.Transaction where
    arbitrary = undefined
