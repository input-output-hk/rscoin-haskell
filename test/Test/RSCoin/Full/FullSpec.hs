{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedLists #-}

-- | HSpec specification of full rscoin.

module Test.RSCoin.Full.FullSpec
       ( spec
       ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad.Extra         (whenJust)
import           Control.Monad.Trans         (lift)

import           Data.IntMap                 ((!))
import           Data.List                   (nub)
import           Test.Hspec                  (Spec, before, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Arbitrary (arbitrary), Property,
                                              property)

import           RSCoin.Core                 (CoinsMap, Color (..), WorkMode,
                                              bankLoggerName, getNodeContext,
                                              grey, initLoggerByName,
                                              initLogging, mintetteLoggerName,
                                              testingLoggerName, userLoggerName)
import qualified RSCoin.User                 as U

import           System.IO.Unsafe            (unsafePerformIO)
import           Test.QuickCheck             (NonEmptyList (..))
import           Test.RSCoin.Full.Action     (PartsToSend (..), UserAction (..), applyPartsToSend,
                                              getUserState)
import           Test.RSCoin.Full.Property   (FullPropertyEmulation,
                                              FullPropertyRealMode, assertFP,
                                              doActionFP, pickFP, runTestEnvFP,
                                              runWorkModeFP)
import qualified Test.RSCoin.Full.Property   as FP (FullProperty)
import           TestOptions                 (FullTestConfig (..), testTVar)

spec :: Spec
spec = do
    before (setupLogging cfg)$ do
        describe "Full RSCoin" $ do
            fullProp uniqueAdrDesc prop_uniqueAddresses
            fullProp sendLoopDesc prop_sendLoopBack
            fullProp send2inARowDesc prop_send2inARow
  where
    cfg@FullTestConfig{..} = unsafePerformIO $ readTVarIO testTVar
    fullProp :: String -> FullProperty -> Spec
    fullProp propDescr = prop propDescr . propConverter
    propConverter :: FullProperty -> Property
    propConverter =
        if ftcRealMode
            then (property :: FullPropertyRealMode a -> Property)
            else (property :: FullPropertyEmulation a -> Property)
    uniqueAdrDesc = "all users have unique addresses"
    sendLoopDesc = "sending an amount from an address to itself does not " ++
                   "change its balance"
    send2inARowDesc = "sending some coins from one address to another, and " ++
                      "from it to another does not leave any along the way"
{-# NOINLINE spec#-}

setupLogging :: FullTestConfig -> IO ()
setupLogging FullTestConfig{..} = do
    initLogging ftcGlobalSeverity
    whenJust ftcBankSeverity $ flip initLoggerByName bankLoggerName
    whenJust ftcMintetteSeverity $ flip initLoggerByName mintetteLoggerName
    whenJust ftcUserSeverity $ flip initLoggerByName userLoggerName
    whenJust ftcTestingSeverity $ flip initLoggerByName testingLoggerName

type FullProperty = forall m . WorkMode m => FP.FullProperty m ()

prop_uniqueAddresses :: FullProperty
prop_uniqueAddresses = do
    nodeCtx <- lift $ lift $ getNodeContext
    idx <- pickFP arbitrary
    st <- runTestEnvFP $ getUserState idx
    assertFP . isUnique =<< runWorkModeFP (U.getAllAddresses st nodeCtx)
  where
    isUnique l = l == nub l

getAmount :: WorkMode m
          => U.UserState
          -> Int
          -> FP.FullProperty m CoinsMap
getAmount st i = runWorkModeFP $ U.getAmountByIndex st i

prop_sendLoopBack :: FullProperty
prop_sendLoopBack = do
    state <- runTestEnvFP $ getUserState Nothing
    amount <- getAmount state 1
    addr <- head <$> (runWorkModeFP $ U.getAllPublicAddresses state)
    let partsToSend = [(0, 0.5)]
    doActionFP $
        SubmitTransaction
            Nothing
            (NonEmpty [(0, partsToSend)])
            (Left addr)
            Nothing
    amount' <- getAmount state 1
    assertFP $ amount == amount'

-- TODO
prop_send2inARow :: FullProperty
prop_send2inARow = do
    state <- runTestEnvFP $ getUserState Nothing
    addrs <- runWorkModeFP $ U.getAllPublicAddresses state
    amount0 <- getAmount state 0
    -- amount1 <- getAmount state 1
    -- amount2 <- getAmount state 2
    let partsToSend :: PartsToSend
        partsToSend = [(0, 0.1)]
        sent = applyPartsToSend partsToSend amount0
        sentGrey = sent ! gr
    doActionFP $
        SubmitTransaction
            Nothing
            (NonEmpty [(0, partsToSend)])
            (Left $ addrs !! 1)
            Nothing
    -- doActionFP $
    --     SubmitTransaction
    --         Nothing
    --         (NonEmpty [(1, partsToSend)])
    --         (Left $ addrs !! 2)
    --         Nothing
    amount0' <- getAmount state 0
    -- amount1' <- getAmount state 1
    -- amount2' <- getAmount state 2
    assertFP $ (amount0 ! gr) - (amount0' ! gr) == sentGrey
    -- assertFP $ (amount2' ! gr) - (amount2 ! gr) == sentGrey
    -- assertFP $ amount1' == amount1
  where
    gr = getColor grey
