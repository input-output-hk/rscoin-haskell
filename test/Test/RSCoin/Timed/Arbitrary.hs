-- | Arbitrary instances for Timed types.

module Test.RSCoin.Timed.Arbitrary
       (
       ) where

import           Data.Time.Units (fromMicroseconds)
import           Test.QuickCheck (Arbitrary (arbitrary))

import qualified RSCoin.Timed    as T

instance Arbitrary T.Microsecond where
    arbitrary = fromMicroseconds <$> arbitrary

instance Arbitrary T.Millisecond where
    arbitrary = fromMicroseconds <$> arbitrary

instance Arbitrary T.Second where
    arbitrary = fromMicroseconds <$> arbitrary

instance Arbitrary T.Minute where
    arbitrary = fromMicroseconds <$> arbitrary
