{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Arbitrary instances for full testing.

module Test.RSCoin.Full.Gen
       ( genValidActions
       ) where

import           Data.Bifunctor                  (first)
import           Data.Either.Combinators         (mapRight)
import qualified Data.IntMap.Strict              as M
import           Data.Time.Units                 (fromMicroseconds)
import           Test.QuickCheck                 (Arbitrary (arbitrary), Gen,
                                                  choose, listOf, shuffle,
                                                  sized, sublistOf, suchThat,
                                                  vectorOf)

import qualified RSCoin.Core                     as C

import           Test.RSCoin.Full.Action         (Coloring (Coloring),
                                                  PartToSend (..),
                                                  PartsToSend (..),
                                                  SomeAction (SomeAction),
                                                  ToAddress, UserAction (..),
                                                  UserIndex, WaitAction (..))
import           Test.RSCoin.Full.Constants      (maxColor, minColor)
import           Test.RSCoin.Full.Context        (MintetteNumber, UserNumber)
import           Test.RSCoin.Full.Initialization (bankUserAddressesCount,
                                                  userAddressesCount)
import           Test.RSCoin.Local.Arbitrary     ()

instance Arbitrary MintetteNumber where
    arbitrary = choose (1, 5)

instance Arbitrary UserNumber where
    arbitrary = choose (1, 7)

instance Arbitrary PartToSend where
    arbitrary = PartToSend <$> choose (0.001, 1.0)

instance Arbitrary PartsToSend where
    arbitrary = do
        colors <- sublistOf [minColor .. maxColor] `suchThat` (not . null)
        PartsToSend . M.fromList <$>
            mapM
                (\color ->
                      (C.getC color, ) <$> arbitrary)
                colors

genWaitAction :: a -> Gen (WaitAction a)
genWaitAction a =
    WaitAction <$> (fromMicroseconds <$> choose (0, 15 * 1000 * 1000)) <*>
    pure a -- at most 15 seconds

genColor :: Gen C.Color
genColor = C.Color <$> choose (C.getC minColor, C.getC maxColor)

instance Arbitrary Coloring where
    arbitrary = do
        colors <- listOf (genColor `suchThat` (/= 0))
        parts <- vectorOf (length colors) (choose (1.0e-4, 1.0))
        targetSum <- choose (0.1, 1.0)
        let s = sum parts
            multiplier = targetSum / s
        return .
            Coloring . M.fromListWith (+) . zip (map C.getC colors) . map (* multiplier) $
            parts

genUserIndex :: UserNumber -> Gen UserIndex
genUserIndex un = fmap (`mod` fromIntegral un) <$> arbitrary

genToAddress :: UserNumber -> Gen ToAddress
genToAddress (fromIntegral -> userNumber) = mapRight fixIndices <$> arbitrary
  where
    fixIndices (usrIdx,addrIdx) =
        ( (`mod` userNumber) <$> usrIdx
        , maybe
              (`mod` bankUserAddressesCount)
              (const (`mod` userAddressesCount))
              usrIdx
              addrIdx)

genSubmitTransaction :: UserNumber -> Gen UserAction
genSubmitTransaction un = do
    usrIdx <- genUserIndex un
    let addressesCount =
            maybe bankUserAddressesCount (const userAddressesCount) usrIdx
        fixFromAddress = first (`mod` addressesCount)
    SubmitTransaction usrIdx <$> (fmap fixFromAddress <$> arbitrary) <*>
        genToAddress un <*>
        arbitrary

genUpdateBlockchain :: Gen UserAction
genUpdateBlockchain = UpdateBlockchain <$> arbitrary

type ActionsDescription = [SomeAction]

-- | Generate sequence of actions which can be applied to empty context
-- (created using mkTestContext) and are guaranteed to be executed
-- without fails.
genValidActions :: UserNumber -> Gen ActionsDescription
genValidActions userNumber = do
    userActions <- map SomeAction <$> sized genUserActions
    actions <- mapM genWaitAction userActions
    return (map SomeAction actions)
  where
    genUserActions s = do
        let updatesNum = s `div` 10
            transactionsNum = s - updatesNum
        updates <- vectorOf updatesNum genUpdateBlockchain
        transactions <-
            vectorOf transactionsNum $ genSubmitTransaction userNumber
        shuffle $ updates ++ transactions
