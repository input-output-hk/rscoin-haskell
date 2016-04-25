{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | RSCoin.Test.MonadRpc specification

module RSCoin.Test.MonadRpcSpec
       ( spec
       ) where

import           Control.Concurrent.MVar     (newEmptyMVar, takeMVar, putMVar)
import           Control.Monad.Trans         (liftIO, lift, MonadIO)
import           Data.MessagePack.Object     (Object(..), MessagePack, 
                                              toObject)
import qualified Data.ByteString             as B
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Numeric.Natural             (Natural)
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Arbitrary (arbitrary), 
                                             Property, ioProperty,
                                             counterexample, oneof)
import           Test.QuickCheck.Function    (Fun, apply) 
import           Test.QuickCheck.Monadic     (run, assert, PropertyM, monadic,
                                             monitor)
import           Test.QuickCheck.Poly        (A)

import           RSCoin.Test.MonadTimed      (MicroSeconds, now, RelativeToNow,
                                              after, MonadTimed (..), runTimedIO,
                                              TimedIO, schedule, invoke)
import           RSCoin.Test.Timed           (runTimedT, TimedT)

spec :: Spec
spec =
    return ()

instance Arbitrary Object where
    arbitrary = oneof [ pure ObjectNil
                      , ObjectBool <$> arbitrary
                      , ObjectInt <$> arbitrary
                      , ObjectFloat <$> arbitrary
                      , ObjectDouble <$> arbitrary
                      , ObjectStr <$> arbitrary
                      , ObjectBin <$> arbitrary
                      , ObjectArray <$> arbitrary
                      , ObjectMap <$> arbitrary
                      , ObjectExt <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = V.fromList <$> arbitrary
