{-# LANGUAGE ScopedTypeVariables #-}

-- | Arbitrary instances for full testing.

module Test.RSCoin.Full.Gen
       ( genActions
       ) where

import           Data.Time.Units             (addTime)
import           Test.QuickCheck             (Arbitrary (arbitrary), Gen,
                                              NonNegative (..), frequency,
                                              oneof)

import           RSCoin.Timed                (Microsecond, minute)

import           Test.RSCoin.Core.Arbitrary  ()
import           Test.RSCoin.Full.Action     (InitAction (InitAction),
                                              SomeAction (SomeAction),
                                              UserAction (..), WaitAction (..),
                                              WaitSomeAction)
import           Test.RSCoin.Full.Context    (MintetteNumber, UserNumber)
import           Test.RSCoin.Timed.Arbitrary ()

instance Arbitrary MintetteNumber where
    arbitrary = pure 1

instance Arbitrary UserNumber where
    arbitrary = pure 1

instance Arbitrary a => Arbitrary (WaitAction a) where
    arbitrary = WaitAction <$> arbitrary <*> arbitrary

instance Arbitrary UserAction where
    arbitrary =
        frequency [ (10, FormTransaction <$> arbitrary <*> arbitrary <*> arbitrary)
                  , (1, ListAddresses <$> arbitrary)
                  , (10, UpdateBlockchain <$> arbitrary)
                  ]

-- TODO: maybe we should create also StartMintette, AddMintette, in terms of actions
instance Arbitrary SomeAction where
    arbitrary = oneof [SomeAction <$> (arbitrary :: Gen UserAction)]

genActions :: Gen ([SomeAction], Microsecond)
genActions = do
    actions :: [WaitSomeAction] <- arbitrary
    let actionsRunningTime = sum $ map (\(WaitAction t _) -> getNonNegative t) actions
        safeRunningTime = addTime actionsRunningTime (minute 1)
    return (SomeAction InitAction : map SomeAction actions, safeRunningTime)
