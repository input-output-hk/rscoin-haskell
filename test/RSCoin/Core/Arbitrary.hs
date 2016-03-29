-- | Arbitrary instances for Core types.

module RSCoin.Core.Arbitrary
       (
       ) where

import           Test.QuickCheck (Arbitrary (arbitrary))

import qualified RSCoin.Core     as C

instance Arbitrary C.Coin where
    arbitrary = C.Coin <$> arbitrary

instance Arbitrary C.Mintette where
    arbitrary = C.Mintette <$> arbitrary <*> arbitrary
