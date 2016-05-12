{-# LANGUAGE ScopedTypeVariables #-}

-- | Arbitrary instances for full testing.

module Test.RSCoin.Full.Gen
       ( genActions
       ) where

import           Data.Time.Units             (addTime)
import           Test.QuickCheck             (Arbitrary (arbitrary), Gen,
                                              frequency, oneof)

import           RSCoin.Timed                (Microsecond)

import           Test.RSCoin.Core.Arbitrary  ()
import           Test.RSCoin.Full.Action     (InitAction (InitAction),
                                              SomeAction (SomeAction),
                                              UserAction (..), WaitAction (..),
                                              WaitSomeAction, doAction)
import           Test.RSCoin.Timed.Arbitrary ()

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
    arbitrary = oneof [ SomeAction <$> (arbitrary :: Gen UserAction)
                      ]

-- TODO: it should be expanded akin to definition below.
genActions :: Gen ([SomeAction], Microsecond)
genActions = pure ([SomeAction InitAction], 0)

-- instance WorkMode m => Arbitrary (WorkTestContext m) where
--     arbitrary = do
--         actions :: [WaitSomeAction] <- arbitrary
--         let actionsRunningTime = sum $ map (\(WaitAction t _) -> getNonNegative t) actions
--             safeRunningTime = addTime actionsRunningTime (minute 1)
--         mNum <- arbitrary
--         uNum <- arbitrary
--         return $ WorkTestContext $ (mkTestContext mNum uNum safeRunningTime >>=) $ runReaderT $ do
--             runBank
--             mapM_ runMintette =<< view mintettes

--             wait $ for 5 sec  -- ensure that bank & mintettes have initialized

--             mapM_ addMintetteToBank =<< view mintettes
--             initBUser
--             mapM_ initUser =<< view users

--             wait $ for 5 sec  -- ensure that users have initialized

--             mapM_ doAction actions

--             wait $ for safeRunningTime mcs -- wait for all actions to finish

--             ask
