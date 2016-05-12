{-# LANGUAGE ScopedTypeVariables #-}

-- | Arbitrary instances for full testing.

module Test.RSCoin.Full.Arbitrary
       (
       ) where

import           Control.Lens                (view, (^.))
import           Control.Monad.Reader        (ask, runReaderT)
import           Control.Monad.Trans         (MonadIO)
import           Data.Acid.Advanced          (update')
import           Data.Time.Units             (addTime)
import           Test.QuickCheck             (Arbitrary (arbitrary), Gen,
                                              NonNegative (..), frequency, oneof)

import qualified RSCoin.Bank                 as B
import           RSCoin.Core                 (Mintette (..))
import qualified RSCoin.Mintette             as M
import           RSCoin.Timed                (WorkMode, for, mcs, minute, sec,
                                              upto, wait, work)
import qualified RSCoin.User                 as U

import           Test.RSCoin.Core.Arbitrary  ()
import           Test.RSCoin.Timed.Arbitrary ()
import           Test.RSCoin.Full.Action     (EmptyAction (..),
                                              SomeAction (SomeAction),
                                              UserAction (..), WaitAction (..),
                                              WaitSomeAction, doAction)
import           Test.RSCoin.Full.Context    (MintetteInfo, TestEnv, UserInfo,
                                              WorkTestContext (WorkTestContext),
                                              bank, bankSkPath, buser, lifetime,
                                              mintettes, mkTestContext, port,
                                              publicKey, secretKey, state, users)

instance Arbitrary EmptyAction where
    arbitrary = pure EmptyAction

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

instance WorkMode m => Arbitrary (WorkTestContext m) where
    arbitrary = do
        actions :: [WaitSomeAction] <- arbitrary
        let actionsRunningTime = sum $ map (\(WaitAction t _) -> getNonNegative t) actions
            safeRunningTime = addTime actionsRunningTime (minute 1)
        mNum <- arbitrary
        uNum <- arbitrary
        return $ WorkTestContext $ (mkTestContext mNum uNum safeRunningTime >>=) $ runReaderT $ do
            runBank
            mapM_ runMintette =<< view mintettes

            wait $ for 5 sec  -- ensure that bank & mintettes have initialized

            mapM_ addMintetteToBank =<< view mintettes
            initBUser
            mapM_ initUser =<< view users

            wait $ for 5 sec  -- ensure that users have initialized

            mapM_ doAction actions

            wait $ for safeRunningTime mcs -- wait for all actions to finish

            ask

runBank :: WorkMode m => TestEnv m ()
runBank = do
    b <- view bank
    l <- view lifetime
    work (upto l mcs) $ B.runWorker (b ^. secretKey) (b ^. state)
    work (upto l mcs) $ B.serve (b ^. state)

runMintette :: WorkMode m => MintetteInfo -> TestEnv m ()
runMintette m = do
    l <- view lifetime
    work (upto l mcs) $
        M.serve <$> view port <*> view state <*> view secretKey $ m
    work (upto l mcs) $
        M.runWorker <$> view secretKey <*> view state $ m

addMintetteToBank :: MonadIO m => MintetteInfo -> TestEnv m ()
addMintetteToBank mintette = do
    let addedMint = Mintette "127.0.0.1" (mintette ^. port)
        mintPKey  = mintette ^. publicKey
    bankSt <- view $ bank . state
    update' bankSt $ B.AddMintette addedMint mintPKey

initBUser :: WorkMode m => TestEnv m ()
initBUser = do
    st <- view $ buser . state
    skPath <- bankSkPath
    U.initState st 5 (Just skPath)

initUser :: WorkMode m => UserInfo -> TestEnv m ()
initUser user = U.initState (user ^. state) 5 Nothing
