module Test.RSCoin.Full.Arbitrary
       (
       ) where

import           Data.Time.Units                   (TimeUnit, convertUnit,
                                                    fromMicroseconds)
import           Test.QuickCheck                   (Arbitrary (arbitrary), choose)
import           System.Random                     (StdGen, mkStdGen)

import qualified Control.TimeWarp.Timed.MonadTimed as T


convertMicroSecond :: TimeUnit t => T.Microsecond -> t
convertMicroSecond = convertUnit

instance Arbitrary T.Microsecond where
    -- no more than 10 minutes
    arbitrary = fromMicroseconds <$> choose (0, 600 * 1000 * 1000)

instance Arbitrary T.Millisecond where
    arbitrary = convertMicroSecond <$> arbitrary

instance Arbitrary T.Second where
    arbitrary = convertMicroSecond <$> arbitrary

instance Arbitrary T.Minute where
    arbitrary = convertMicroSecond <$> arbitrary

instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary
