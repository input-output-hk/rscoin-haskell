{-# LANGUAGE RankNTypes   #-}

-- | HSpec specification of full rscoin.

module Test.RSCoin.Full.FullSpec
       ( spec
       ) where

import           Control.Concurrent.MVar    (MVar, newEmptyMVar, putMVar,
                                             readMVar)
import           Control.Lens               (view, (^.))
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.Acid.Advanced         (update')
import           Data.Int                   (Int64)
import           Test.Hspec                 (Spec, describe, it, pending)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen,
                                             NonEmptyList (..),
                                             NonNegative (..), Property,
                                             frequency, oneof)
import           Test.QuickCheck.Monadic    (assert, monadicIO)

import           RSCoin.Core                (Address (..), Coin (..),
                                             Mintette (..))
import qualified RSCoin.Mintette            as M
import           RSCoin.Timed               (MicroSeconds, WorkMode, for,
                                             invoke, mcs, minute, sec, upto,
                                             wait, work)
import qualified RSCoin.User                as U

import           Test.RSCoin.Core.Arbitrary ()
import           Test.RSCoin.Full.Action    (SomeAction (..), UserAction (..),
                                             WaitAction (WaitAction),
                                             WaitSomeAction, doAction)
import           Test.RSCoin.Full.Arbitrary ()
import           Test.RSCoin.Full.Context   (MintetteInfo, TestContext, TestEnv,
                                             UserInfo,
                                             WorkTestContext (WorkTestContext),
                                             bank, bankSkPath, buser, lifetime,
                                             mintettes, mkTestContext, port,
                                             publicKey, secretKey, state, users)

spec :: Spec
spec =
    describe "RSCoin" $ do
        it "is a great cryptocurrency" $ do
            pending

runAnotherAction
    :: WorkMode m
    => WorkTestContext m -> SomeAction -> WorkTestContext m
runAnotherAction (WorkTestContext context) action =
    WorkTestContext $ context >>= runReaderT (doAction action >> ask)

-- TODO: this is a workaround until exception handling is fixed (waiting until @martoon merges).
-- We cant pull out value from PureRpc so one solution is to use create something like PureRpc (State Bool) and use that for testing. This solution is used in MonadRpcSpec module. Because PureRpc is MonadIO here, I am instead reusing MVar. Possibly better approach would be to throw some SpecialTestException and catch that exception outside. If exception is caught then test failed.
monadicIOProp :: WorkMode m => ReaderT (MVar Bool) m () -> Property
monadicIOProp action = monadicIO $ do
    var <- liftIO newEmptyMVar
    -- FIXME: run action >> putMVar True
    res <- liftIO $ readMVar var
    assert res

assertProp :: WorkMode m => Bool -> ReaderT (MVar Bool) m ()
assertProp True  = pure ()
assertProp False = ask >>= liftIO . flip putMVar False

somePropertyX :: WorkMode m => WorkTestContext m -> Property
somePropertyX state = monadicIO $
    -- assert $ 1 == 1
    return ()

somePropertyAfterAction :: WorkMode m => WorkTestContext m -> SomeAction -> Property
somePropertyAfterAction state action = monadicIO $
    -- runAnotherAction state action
    return ()
