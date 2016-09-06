{-# LANGUAGE FlexibleContexts     #-}
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

import           Control.Monad.Catch             (onException)
import           Control.Monad.Reader            (ask, runReaderT)
import           Control.Monad.Trans             (MonadIO, lift)
import           Formatting                      (build, sformat, (%))
import           System.Random                   (StdGen)
import           Test.QuickCheck                 (Gen, Property,
                                                  Testable (property),
                                                  ioProperty)
import           Test.QuickCheck.Monadic         (PropertyM, assert, monadic,
                                                  pick)

import           Serokell.Util                   (listBuilderJSONIndent)

import           Control.TimeWarp.Rpc            (Delays)
import           RSCoin.Core                     (ContextArgument (CADefault),
                                                  EmulationMode, RealMode,
                                                  WithNamedLogger (..),
                                                  WorkMode, logDebug,
                                                  runEmulationMode,
                                                  runRealModeUntrusted,
                                                  testingLoggerName)

import           Test.RSCoin.Full.Action         (Action (doAction))
import           Test.RSCoin.Full.Context        (MintetteNumber,
                                                  Scenario (DefaultScenario),
                                                  TestEnv, UserNumber)
import           Test.RSCoin.Full.Gen            (genValidActions)
import           Test.RSCoin.Full.Initialization (finishTest, mkTestContext)

type FullProperty m = TestEnv (PropertyM m)
type FullPropertyEmulation = FullProperty EmulationMode
type FullPropertyRealMode = FullProperty RealMode

launchPure :: StdGen -> Delays -> EmulationMode a -> IO a
launchPure gen = runEmulationMode (Just gen)

launchReal :: RealMode a -> IO a
launchReal = runRealModeUntrusted testingLoggerName CADefault

instance (WithNamedLogger m, MonadIO m) =>
         WithNamedLogger (PropertyM m) where
    getLoggerName = lift getLoggerName

toPropertyM
    :: WorkMode m
    => FullProperty m a -> MintetteNumber -> UserNumber -> PropertyM m a
toPropertyM fp mNum uNum = do
    acts <- pick $ genValidActions uNum
    context <- lift $ mkTestContext mNum uNum DefaultScenario
    let runTestEnv a = runReaderT a context
        runTestEnvSafe a = runTestEnv a `onException` runTestEnv finishTest
    logDebug $
        sformat ("Actions are: " % build) $ listBuilderJSONIndent 3 acts
    lift $ runTestEnvSafe (mapM_ doAction acts)
    runReaderT fp context <* lift (runTestEnv finishTest)

toTestable
    :: forall a.
       forall m. WorkMode m => (forall b. m b -> IO b) -> FullProperty m a -> MintetteNumber -> UserNumber -> Property
toTestable launcher fp mNum uNum = monadic unwrapProperty wrappedProperty
  where
    (unwrapProperty :: m Property -> Property) = ioProperty . launcher
    (wrappedProperty :: PropertyM m a) = toPropertyM fp mNum uNum

instance Testable (FullPropertyEmulation a) where
   property fp =
       property $
       \gen ->
            \delays ->
                 toTestable (launchPure gen delays) fp

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
runWorkModeFP a = do
    ctx <- ask
    lift . lift $ a `onException` runReaderT finishTest ctx

runTestEnvFP
    :: WorkMode m
    => TestEnv m a -> FullProperty m a
runTestEnvFP a = runWorkModeFP . runReaderT a =<< ask

doActionFP
    :: (WorkMode m, Action a)
    => a -> FullProperty m ()
doActionFP = runTestEnvFP . doAction
