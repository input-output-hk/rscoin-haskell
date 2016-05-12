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
import           Data.Word                  (Word16, Word8)
import           Test.Hspec                 (Spec, describe, it, pending)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Property, Testable (property),
                                             ioProperty)
import           Test.QuickCheck.Monadic    (PropertyM, monadic, pick)
import qualified Test.QuickCheck.Monadic    as TQM (assert)

import           RSCoin.Core                (Address (..), Coin (..),
                                             Mintette (..), genesisAddress)
import           RSCoin.Timed               (Microsecond, MonadRpc (..),
                                             MonadTimed (..), PureRpc, WorkMode,
                                             for, invoke, mcs, minute,
                                             runEmulationMode, sec, upto, wait,
                                             work)
import qualified RSCoin.User                as U

import           Test.RSCoin.Core.Arbitrary ()
import           Test.RSCoin.Full.Action    (Action (doAction), UserAction (..),
                                             doAction)
import           Test.RSCoin.Full.Context   (MintetteInfo, TestContext, TestEnv,
                                             UserInfo, bank, buser, lifetime,
                                             mintettes, mkTestContext, port,
                                             publicKey, secretKey, state, users)
import           Test.RSCoin.Full.Gen       (genActions)

spec :: Spec
spec =
    describe "RSCoin" $ do
        -- It fails now
        -- prop "great property" dummyProperty
        return ()

launchPure :: MonadIO m => PureRpc IO a -> m a
launchPure = runEmulationMode def def

toTestable :: FullProperty -> Word8 -> Word16 -> Property
toTestable fp mintetteCount userCount =
    monadic unwrapProperty $
    do (acts,t) <- pick genActions
       context <- lift $ mkTestContext mintetteCount userCount t
       launchPure $ runReaderT (mapM_ doAction acts) context
       runReaderT fp context
  where
    unwrapProperty = ioProperty . launchPure

type FullProperty = (TestEnv (PropertyM (PureRpc IO)) ())

instance Testable FullProperty  where
    property = property . toTestable

assert :: Bool -> FullProperty
assert = lift . TQM.assert

runAction :: Action a => a -> FullProperty
runAction action = lift . lift . runReaderT (doAction action) =<< ask

dummyProperty :: FullProperty
dummyProperty = do
    buSt <- view $ buser . state
    amount <- U.getAmountByIndex buSt 1
    runAction $ FormTransaction undefined undefined $ Left genesisAddress
    amount' <- U.getAmountByIndex buSt 1
    assert $ amount' - amount == 50
