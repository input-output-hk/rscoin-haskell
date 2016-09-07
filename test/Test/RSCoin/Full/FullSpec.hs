{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | HSpec specification of full rscoin.

module Test.RSCoin.Full.FullSpec
       ( FullTestConfig (..)
       , spec
       ) where

import           Control.Monad.Extra       (whenJust)
import           Control.Monad.Trans       (lift)

import           Data.Default              (Default (def))
import           Data.IntMap               (fromList, (!))
import           Data.List                 (nub)
import           Test.Hspec                (Spec, before, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (Arbitrary (arbitrary), Property,
                                            property)

import           RSCoin.Core               (CoinsMap, Color (..), Severity (..),
                                            WorkMode, bankLoggerName,
                                            getNodeContext, grey,
                                            initLoggerByName, initLogging,
                                            mintetteLoggerName,
                                            testingLoggerName, userLoggerName)
import qualified RSCoin.User               as U

import           Test.QuickCheck           (NonEmptyList (..))
import           Test.RSCoin.Full.Action   (PartsToSend (..), UserAction (..),
                                            getUserState)
import           Test.RSCoin.Full.Property (FullPropertyEmulation,
                                            FullPropertyRealMode, assertFP,
                                            doActionFP, pickFP, runTestEnvFP,
                                            runWorkModeFP)
import qualified Test.RSCoin.Full.Property as FP (FullProperty)

data FullTestConfig = FullTestConfig
    { ftcGlobalSeverity   :: !Severity
    , ftcBankSeverity     :: !(Maybe Severity)
    , ftcMintetteSeverity :: !(Maybe Severity)
    , ftcUserSeverity     :: !(Maybe Severity)
    , ftcTestingSeverity  :: !(Maybe Severity)
    , ftcRealMode         :: !Bool
    } deriving (Show)

instance Default FullTestConfig where
    def =
        FullTestConfig
        { ftcGlobalSeverity = Warning
        , ftcBankSeverity = def
        , ftcMintetteSeverity = def
        , ftcUserSeverity = def
        , ftcTestingSeverity = Just Warning
        , ftcRealMode = False
        }

spec :: Spec
spec =
    before (setupLogging cfg) $ do
        describe "Full RSCoin" $ do
            fullProp uniqueAdrDesc prop_uniqueAddresses
            fullProp sendLoopDesc prop_sendLoopBack
            fullProp send2inARowDesc prop_send2inARow
  where
    cfg@FullTestConfig {..} = def
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
    doActionFP $ SubmitTransaction Nothing
                                   (NonEmpty [(1, PartsToSend $ fromList [(0, 50.0)])])
                                   (Left addr)
                                   Nothing
    amount' <- getAmount state 1
    assertFP $ amount == amount'

prop_send2inARow :: FullProperty
prop_send2inARow = do
    state <- runTestEnvFP $ getUserState Nothing
    addrs <- runWorkModeFP $ U.getAllPublicAddresses state
    amount1 <- getAmount state 1
    amount2 <- getAmount state 2
    amount3 <- getAmount state 3
    doActionFP $ SubmitTransaction Nothing
                                   (NonEmpty [(1, PartsToSend $ fromList [(0, 50.0)])])
                                   (Left $ addrs !! 2)
                                   Nothing
    doActionFP $ SubmitTransaction Nothing
                                   (NonEmpty [(2, PartsToSend $ fromList [(0, 50.0)])])
                                   (Left $ addrs !! 3)
                                   Nothing
    amount1' <- getAmount state 1
    amount2' <- getAmount state 2
    amount3' <- getAmount state 3
    assertFP $ (amount1 ! gr) - (amount1' ! gr) == 50
    assertFP $ (amount3' ! gr) - (amount3 ! gr) == 50
    assertFP $ amount2' == amount2
  where
    gr = getC grey
