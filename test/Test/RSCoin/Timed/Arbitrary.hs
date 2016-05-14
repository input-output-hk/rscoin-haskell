-- | Arbitrary instances for Timed types.

module Test.RSCoin.Timed.Arbitrary
       (
       ) where

import           Control.Monad.Random.Class  (getRandomR)
import           Data.Time.Units             (fromMicroseconds)
import           Test.QuickCheck             (Arbitrary (arbitrary))
import           System.Random               (StdGen, mkStdGen)

import qualified RSCoin.Timed    as T

instance Arbitrary T.Microsecond where
    arbitrary = fromMicroseconds <$> arbitrary

instance Arbitrary T.Millisecond where
    arbitrary = fromMicroseconds <$> arbitrary

instance Arbitrary T.Second where
    arbitrary = fromMicroseconds <$> arbitrary

instance Arbitrary T.Minute where
    arbitrary = fromMicroseconds <$> arbitrary

-- TODO: this is kind of odd
instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary

instance Arbitrary T.Delays where
    arbitrary =
        return $ T.Delays (\_ _ -> Just . fromMicroseconds <$> getRandomR (0, 1000))

instance Show T.Delays where
    show _ = "Random generator"
