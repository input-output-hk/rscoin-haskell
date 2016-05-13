{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | HSpec specification of full rscoin.

module Test.RSCoin.Full.FullSpec
       ( spec
       ) where

import           Control.Lens               (view)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (arbitrary),
                                             NonEmptyList (NonEmpty))

import qualified RSCoin.User                as U

import           Test.RSCoin.Core.Arbitrary ()
import           Test.RSCoin.Full.Action    (UserAction (..))
import           Test.RSCoin.Full.Context   (buser, state)
import           Test.RSCoin.Full.Property  (FullProperty, assertFP, doActionFP,
                                             pickFP, runWorkModeFP)

spec :: Spec
spec =
    describe "RSCoin" $ do
        -- It fails now
        prop "great property" dummyProperty

dummyProperty :: FullProperty ()
dummyProperty = do
    buSt <- view $ buser . state
    amount <- runWorkModeFP $ U.getAmountByIndex buSt 1
    addr <- pickFP arbitrary
    doActionFP $ FormTransaction Nothing (NonEmpty [(1, 50)]) $ Left addr
    amount' <- runWorkModeFP $ U.getAmountByIndex buSt 1
    assertFP $ amount' - amount == 50
