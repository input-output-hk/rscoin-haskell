{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | HSpec specification of full rscoin.

module Test.RSCoin.Full.FullSpec
       ( spec
       ) where

import           Control.Lens               (view)
import           Control.Monad.Reader       (ask, runReaderT)
import           Control.Monad.Trans        (MonadIO, lift)
import           Data.Default               (def)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen,
                                             NonEmptyList (NonEmpty), Property,
                                             Testable (property), ioProperty)
import           Test.QuickCheck.Monadic    (PropertyM, assert, monadic, pick)

import           RSCoin.Timed               (Microsecond, MonadRpc (..),
                                             MonadTimed (..), PureRpc, WorkMode,
                                             for, invoke, mcs, minute,
                                             runEmulationMode, sec, upto, wait,
                                             work)
import qualified RSCoin.User                as U

import           Test.RSCoin.Core.Arbitrary ()
import           Test.RSCoin.Full.Action    (Action (doAction), UserAction (..),
                                             doAction)
import           Test.RSCoin.Full.Context   (MintetteNumber, TestEnv,
                                             UserNumber, bank, buser, lifetime,
                                             mintettes, mkTestContext,
                                             publicKey, secretKey, state, users)
import           Test.RSCoin.Full.Gen       (genActions)

spec :: Spec
spec =
    describe "RSCoin" $ do
        -- It fails now
        prop "great property" dummyProperty

launchPure :: MonadIO m => PureRpc IO a -> m a
launchPure = runEmulationMode def def

toTestable :: FullProperty a -> MintetteNumber -> UserNumber -> Property
toTestable fp mNum uNum =
    monadic unwrapProperty $
    do (acts,t) <- pick genActions
       context <- lift $ mkTestContext mNum uNum t
       launchPure $ runReaderT (mapM_ doAction acts) context
       runReaderT fp context
  where
    unwrapProperty = ioProperty . launchPure

type FullProperty a = TestEnv (PropertyM (PureRpc IO)) a

instance Testable (FullProperty a)  where
    property = property . toTestable

assertFP :: Bool -> FullProperty ()
assertFP = lift . assert

pickFP :: (Show a) => Gen a -> FullProperty a
pickFP = lift . pick

runAction :: Action a => a -> FullProperty ()
runAction action = lift . lift . runReaderT (doAction action) =<< ask

dummyProperty :: FullProperty ()
dummyProperty = do
    buSt <- view $ buser . state
    amount <- U.getAmountByIndex buSt 1
    addr <- pickFP arbitrary
    runAction $ FormTransaction Nothing (NonEmpty [(1, 50)]) $ Left addr
    amount' <- U.getAmountByIndex buSt 1
    assertFP $ amount' - amount == 50
