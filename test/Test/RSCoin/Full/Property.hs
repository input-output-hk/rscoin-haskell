{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This module defines data type and some helpers to facilitate
-- properties creation.

module Test.RSCoin.Full.Property
       ( FullProperty
       , FullPropertyEmulation
       , FullPropertyRealMode
       , launchPure
       , toTestable
       , assertFP
       , pickFP
       , runWorkModeFP
       , runTestEnvFP
       , doActionFP
       ) where

import           Control.Monad.Reader       (ask, runReaderT)
import           Control.Monad.Trans        (lift)
import           Data.Default               (def)
import           Test.QuickCheck            (Gen, Property, Testable (property),
                                             ioProperty)
import           Test.QuickCheck.Monadic    (PropertyM, assert, monadic, pick)

import           RSCoin.Timed               (MsgPackRpc, PureRpc, WorkMode, for,
                                             runEmulationMode, runRealModeLocal,
                                             sec, wait)

import           Test.RSCoin.Core.Arbitrary ()
import           Test.RSCoin.Full.Action    (Action (doAction))
import           Test.RSCoin.Full.Context   (MintetteNumber,
                                             Scenario (DefaultScenario),
                                             TestEnv, UserNumber, mkTestContext)
import           Test.RSCoin.Full.Gen       (extraRunningTime, genValidActions)

type FullProperty m = TestEnv (PropertyM m)
type FullPropertyEmulation = FullProperty (PureRpc IO)
type FullPropertyRealMode = FullProperty MsgPackRpc

launchPure :: PureRpc IO a -> IO a
launchPure = runEmulationMode def def

launchReal :: MsgPackRpc a -> IO a
launchReal = runRealModeLocal

toPropertyM
    :: WorkMode m
    => FullProperty m a -> MintetteNumber -> UserNumber -> PropertyM m a
toPropertyM fp mNum uNum = do
    (acts,t) <- pick $ genValidActions uNum
    context <- lift $ mkTestContext mNum uNum t DefaultScenario
    lift $ runReaderT (mapM_ doAction acts) context
    runReaderT fp context <* (lift . wait $ for extraRunningTime sec)

toTestable
    :: forall a.
       forall m. WorkMode m => (forall b. m b -> IO b) -> FullProperty m a -> MintetteNumber -> UserNumber -> Property
toTestable launcher fp mNum uNum = monadic unwrapProperty wrappedProperty
  where
    (unwrapProperty :: m Property -> Property) = ioProperty . launcher
    (wrappedProperty :: PropertyM m a) = toPropertyM fp mNum uNum

instance Testable (FullPropertyEmulation a) where
    property = property . toTestable launchPure

instance Testable (FullPropertyRealMode a) where
    property = property . toTestable launchReal

assertFP
    :: Monad m
    => Bool -> FullProperty m ()
assertFP = lift . assert

pickFP
    :: (Monad m, Show a)
    => Gen a -> FullProperty m a
pickFP = lift . pick

runWorkModeFP
    :: WorkMode m
    => m a -> FullProperty m a
runWorkModeFP = lift . lift

runTestEnvFP
    :: WorkMode m
    => TestEnv m a -> FullProperty m a
runTestEnvFP a = runWorkModeFP . runReaderT a =<< ask

doActionFP
    :: (WorkMode m, Action a)
    => a -> FullProperty m ()
doActionFP = runTestEnvFP . doAction
