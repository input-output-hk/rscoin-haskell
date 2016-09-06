{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Actions affecting global context.

module Test.RSCoin.Full.Action
       ( Action (..)
       , SomeAction (..)
       , WaitAction (..)
       , WaitSomeAction
       , UserAction (..)
       , UserIndex
       , ToAddress
       , PartToSend (..)
       , PartsToSend (..)
       , Coloring (..)
       , applyPartToSend
       , applyPartsToSend
       , getUserState
       ) where

import           Control.Lens             (view, views)
import           Control.Monad            (unless, void, when)
import           Control.Monad.Catch      (throwM)
import           Data.Bifunctor           (second)
import           Data.Function            (on)
import qualified Data.IntMap.Strict       as M
import           Data.List                (genericLength, nubBy)
import           Data.Text.Buildable      (Buildable (build))
import           Data.Text.Lazy.Builder   (Builder)
import           Formatting               (bprint, builder, int, shown, (%))
import qualified Formatting
import           Test.QuickCheck          (NonEmptyList (..))

import           Serokell.Util            (indexModulo, indexModuloMay,
                                           listBuilderJSON, mapBuilder,
                                           pairBuilder)

import           Control.TimeWarp.Timed   (Millisecond, after, invoke, ms)
import qualified RSCoin.Core              as C
import qualified RSCoin.User              as U

import           Test.RSCoin.Full.Context (TestEnv, buser, state, users)
import           Test.RSCoin.Full.Error   (TestError (TestError))

class Action a where
    doAction :: C.WorkMode m => a -> TestEnv m ()

data SomeAction =
    forall a. (Action a, Show a, Buildable a) => SomeAction a

instance Show SomeAction where
    show (SomeAction a) = show a

instance Action SomeAction where
    doAction (SomeAction a) = doAction a

instance Buildable SomeAction where
    build (SomeAction a) = build a

data WaitAction a = WaitAction Millisecond a
    deriving Show

type WaitSomeAction = WaitAction SomeAction

instance Action a => Action (WaitAction a) where
    doAction (WaitAction time action) =
        invoke (after time ms) $ doAction action

instance Buildable a => Buildable (WaitAction a) where
    build (WaitAction time action) =
        bprint
            ("wait for " % shown % " and then " % Formatting.build)
            time
            action

-- | Nothing represents bank user, otherwise user is selected according
-- to index in the list
type UserIndex = Maybe Word

-- | Address will be either some arbitrary address or some user address
type ToAddress = Either C.Address (UserIndex, Word)

-- | Represents a number in range (0, 1] and determines how much to
-- send from address.
newtype PartToSend = PartToSend
    { getPartToSend :: Double
    } deriving (Show, Num, Eq, Buildable, Fractional)

applyPartToSend :: PartToSend -> C.Coin -> C.Coin
applyPartToSend (PartToSend p) coin =
    coin { C.getCoin = realToFrac $ p * realToFrac (C.getCoin coin)
         }

-- | How much values of each color to send.
newtype PartsToSend = PartsToSend
    { getPartsToSend :: M.IntMap PartToSend
    } deriving (Show)

instance Buildable PartsToSend where
    build = mapBuilder . M.assocs . getPartsToSend

applyPartsToSend :: PartsToSend -> C.CoinsMap -> C.CoinsMap
applyPartsToSend (getPartsToSend -> parts) = M.foldrWithKey step M.empty
  where
    step color coin accum =
        case M.lookup color parts of
            Nothing -> accum
            Just p  -> M.insert color (applyPartToSend p coin) accum

-- | FromAddresses is a non empty list describing which addresses to
-- use as inputs of transaction. It has pairs where first item is an
-- index of address and the second one determines how much to send.
type FromAddresses = NonEmptyList (Word, PartsToSend)

type Inputs = [U.TransactionInput]

-- | This type determines how to color coins. Values in this map state
-- which part of input grey coins should be colored. (c, t) ∈ Coloring
-- iff `t * input grey coins` should be colored to color `c`. Sum of
-- values must belong to `(0, 1]`.
newtype Coloring = Coloring
    { getColoring :: M.IntMap Double
    } deriving (Show)

calculateOutputCoins :: [C.Coin] -> Coloring -> [C.Coin]
calculateOutputCoins (C.coinsToMap -> inputs) (Coloring coloring) =
    C.coinsToList $
    C.mergeCoinsMaps [inputs, addedCoins] `C.subtractCoinsMap` usedCoins
  where
    greyCoins = maybe 0 C.getCoin $ M.lookup 0 inputs
    addedCoins = M.mapWithKey multiply coloring
    multiply color part =
        C.Coin
        { C.getColor = C.Color color
        , C.getCoin = C.CoinAmount (toRational part) * greyCoins
        }
    usedCoins =
        C.coinsToMap [C.Coin 0 . sum . map C.getCoin . M.elems $ addedCoins]

instance Buildable Coloring where
    build = mapBuilder . M.assocs . getColoring

data UserAction
    = SubmitTransaction !UserIndex
                        !FromAddresses
                        !ToAddress
                        !(Maybe Coloring)
    | UpdateBlockchain !UserIndex
    deriving (Show)

instance Action UserAction where
    doAction (SubmitTransaction userIndex fromAddresses toAddr coloring) = do
        address <- toAddress toAddr
        inputs <- toInputs userIndex fromAddresses
        userState <- getUserState userIndex
        let inputCoins = concatMap snd $ inputs
            outputCoins = maybe [] (calculateOutputCoins inputCoins) coloring
            td =
                U.TransactionData
                { U.tdInputs = inputs
                , U.tdOutputAddress = address
                , U.tdOutputCoins = outputCoins
                }
            retries = 100  -- let's assume that we need more than 100
                           -- with negligible probability
        unless (null inputs) $
            void $ U.submitTransactionRetry retries userState Nothing td
    doAction (UpdateBlockchain userIndex) = do
        st <- getUserState userIndex
        void $ U.updateBlockchain st False

userIndexBuilder :: UserIndex -> Builder
userIndexBuilder = maybe "'bank user'" (bprint ("#" % int))

toAddressBuilder :: ToAddress -> Builder
toAddressBuilder (Left _) = "arbitrary address"
toAddressBuilder (Right (usrIdx,addrIdx)) =
    bprint
        ("address #" % int % " of user " % builder)
        addrIdx
        (userIndexBuilder usrIdx)

instance Buildable UserAction where
    build (SubmitTransaction usrIdx (getNonEmpty -> srcs) dst coloring) =
        bprint
            ("submit transaction from addresses " % builder % " of user " %
             builder %
             " to " %
             builder %
             builder)
            srcsBuilder
            (userIndexBuilder usrIdx)
            (toAddressBuilder dst)
            coloringStr
      where
        srcsBuilder = listBuilderJSON . map pairBuilder $ srcs
        coloringStr =
            maybe "" (bprint (" with coloring " % Formatting.build)) coloring
    build (UpdateBlockchain _) = "update someone's blockchain"

toAddress
    :: C.WorkMode m
    => ToAddress -> TestEnv m C.Address
toAddress =
    either return $
    \(userIndex,addressIndex) -> do
        userState <- getUserState userIndex
        publicAddresses <-
            U.query userState .
            U.GetOwnedDefaultAddresses . view C.genesisAddress =<<
            C.getNodeContext
        return $ publicAddresses `indexModulo` addressIndex

toInputs
    :: C.WorkMode m
    => UserIndex -> FromAddresses -> TestEnv m Inputs
toInputs userIndex (getNonEmpty -> fromIndexes) = do
    userState <- getUserState userIndex
    allAddresses <-
        U.query userState . U.GetOwnedDefaultAddresses . view C.genesisAddress =<<
        C.getNodeContext
    addressesAmount <- mapM (U.getAmount userState) allAddresses
    when (null addressesAmount) $
        throwM $ TestError "No public addresses in this user"
    return $
        nubBy ((==) `on` fst) .
        map (second $ filter C.isPositiveCoin) .
        map (second C.coinsToList) .
        map
            (\(i,parts) ->
                  ( i `mod` genericLength allAddresses
                  , applyPartsToSend parts $ addressesAmount `indexModulo` i)) $
        fromIndexes

getUserState
    :: C.WorkMode m
    => UserIndex -> TestEnv m U.UserState
getUserState Nothing =
    view $ buser . state
getUserState (Just index) = do
    mUser <- views users (`indexModuloMay` index)
    maybe (throwM $ TestError "No user in context") (return . view state) mUser
