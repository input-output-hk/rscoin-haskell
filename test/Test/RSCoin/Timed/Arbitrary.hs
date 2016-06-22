-- | Arbitrary instances for Timed types.

module Test.RSCoin.Timed.Arbitrary
       (
       ) where

import           Control.Monad.Random.Class (getRandomR)
import           Data.Time.Units            (TimeUnit, convertUnit,
                                             fromMicroseconds)
import           System.Random              (StdGen, mkStdGen)
import           Test.QuickCheck            (Arbitrary (arbitrary), choose)

import qualified RSCoin.Timed               as T

convertMicroSecond :: TimeUnit t => T.Microsecond -> t
convertMicroSecond = convertUnit

instance Arbitrary T.Microsecond where
    arbitrary = fromMicroseconds <$> choose (0, 600 * 1000 * 1000)  -- no more than 10 minutes

instance Arbitrary T.Millisecond where
    arbitrary = convertMicroSecond <$> arbitrary

instance Arbitrary T.Second where
    arbitrary = convertMicroSecond <$> arbitrary

instance Arbitrary T.Minute where
    arbitrary = convertMicroSecond <$> arbitrary

-- TODO: this is kind of odd
instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary

instance Arbitrary T.Delays where
    arbitrary =
        return $ T.Delays (\_ _ -> Just . fromMicroseconds <$> getRandomR (0, 1000))

instance Show T.Delays where
    show _ = "Random generator"
