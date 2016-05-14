{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This module defines data type and some helpers to facilitate
-- properties creation.

module Test.RSCoin.Full.Property
       ( FullProperty
       , launchPure
       , toTestable
       , assertFP
       , pickFP
       , runWorkModeFP
       , runTestEnvFP
       , doActionFP
       ) where

import           Control.Monad.Reader       (ask, runReaderT)
import           Control.Monad.Trans        (MonadIO, lift)
import           Data.Default               (def)
import           Test.QuickCheck            (Gen, Property, Testable (property),
                                             ioProperty)
import           Test.QuickCheck.Monadic    (PropertyM, assert, monadic, pick)

import           RSCoin.Timed               (PureRpc, runEmulationMode)

import           Test.RSCoin.Core.Arbitrary ()
import           Test.RSCoin.Full.Action    (Action (doAction))
import           Test.RSCoin.Full.Context   (MintetteNumber,
                                             Scenario (DefaultScenario),
                                             TestEnv, UserNumber, mkTestContext)
import           Test.RSCoin.Full.Gen       (genActions)

type FullProperty a = TestEnv (PropertyM (PureRpc IO)) a

launchPure :: MonadIO m => PureRpc IO a -> m a
launchPure = runEmulationMode def def

toTestable :: FullProperty a
           -> MintetteNumber
           -> UserNumber
           -> Property
toTestable fp mNum uNum =
    monadic unwrapProperty $
    do (acts,t) <- pick genActions
       context <- lift $ mkTestContext mNum uNum t DefaultScenario
       lift $ runReaderT (mapM_ doAction acts) context
       runReaderT fp context
  where
    unwrapProperty = ioProperty . launchPure

instance Testable (FullProperty a)  where
    property = property . toTestable

assertFP :: Bool -> FullProperty ()
assertFP = lift . assert

pickFP :: Show a => Gen a -> FullProperty a
pickFP = lift . pick

runWorkModeFP :: PureRpc IO a -> FullProperty a
runWorkModeFP = lift . lift

runTestEnvFP :: TestEnv (PureRpc IO) a -> FullProperty a
runTestEnvFP a = runWorkModeFP . runReaderT a =<< ask

doActionFP :: Action a => a -> FullProperty ()
doActionFP = runTestEnvFP . doAction
