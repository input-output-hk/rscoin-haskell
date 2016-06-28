{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
       , PartsToSend
       , applyPartToSend
       , applyPartsToSend
       , getUser
       ) where

import           Control.Lens             (to, view)
import           Control.Monad            (unless, void, when)
import           Control.Monad.Catch      (throwM)
import           Data.Acid.Advanced       (query')
import           Data.Bifunctor           (second)
import           Data.Function            (on)
import           Data.List                (nubBy)
import qualified Data.Map                 as M
import           Test.QuickCheck          (NonEmptyList (..), NonNegative (..))

import           Serokell.Util            (indexModulo, indexModuloMay)

import qualified RSCoin.Core              as C
import           RSCoin.Timed             (Second, WorkMode, for, invoke, sec)
import qualified RSCoin.User              as U

import           Test.RSCoin.Full.Context (TestEnv, buser, state, users)
import           Test.RSCoin.Full.Error   (TestError (TestError))

class (Show a) => Action a where
    doAction :: WorkMode m => a -> TestEnv m ()

data SomeAction =
    forall a. (Action a, Show a) => SomeAction a

instance Show SomeAction where
    show (SomeAction a) = show a

instance Action SomeAction where
    doAction (SomeAction a) = doAction a

data WaitAction a = WaitAction (NonNegative Second) a
    deriving Show

type WaitSomeAction = WaitAction SomeAction

instance Action a => Action (WaitAction a) where
    doAction (WaitAction (getNonNegative -> time) action) =
        invoke (for time sec) $ doAction action

-- | Nothing represents bank user, otherwise user is selected according
-- to index in the list
type UserIndex = Maybe Word

-- | Address will be either some arbitrary address or some user address
type ToAddress = Either C.Address (UserIndex, Word)

-- | Represents a number in range (0, 1] and determines how much to
-- send from address.
newtype PartToSend = PartToSend
    { getPartToSend :: Double
    } deriving (Show, Num, Eq)

applyPartToSend :: PartToSend -> C.Coin -> C.Coin
applyPartToSend (PartToSend p) coin =
    coin { C.getCoin = toRational p * C.getCoin coin
         }

-- | How much values of each color to send.
type PartsToSend = M.Map C.Color PartToSend

applyPartsToSend :: PartsToSend -> C.CoinsMap -> C.CoinsMap
applyPartsToSend parts = M.foldrWithKey step M.empty
  where
    step color coin accum =
        case M.lookup color parts of
            Nothing -> accum
            Just p -> M.insert color (applyPartToSend p coin) accum

-- | FromAddresses is a non empty list describing which addresses to
-- use as inputs of transaction. It has pairs where first item is an
-- index of address and the second one determines how much to send.
type FromAddresses = NonEmptyList (Word, PartsToSend)

type Inputs = [U.FormTransactionInput]

data UserAction
    = FormTransaction UserIndex FromAddresses ToAddress
    | UpdateBlockchain UserIndex
    deriving Show

instance Action UserAction where
    doAction (FormTransaction userIndex fromAddresses toAddr) = do
        address <- toAddress toAddr
        inputs <- toInputs userIndex fromAddresses
        user <- getUser userIndex
        let ftd =
                U.FormTransactionData
                { U.ftdInputs = inputs
                , U.ftdOutputAddress = address
                , U.ftdOutputCoins = []
                }
            retries = 5000  -- let's assume that we need more than
                            -- 5000 with negligible probability
        unless (null inputs) $
            void $ U.formTransactionRetry retries user Nothing ftd
    -- FIXME: add a case where we can generate outputs that are not the same as inputs
    doAction (UpdateBlockchain userIndex) = do
        user <- getUser userIndex
        void $ U.updateBlockchain user False

toAddress
    :: WorkMode m
    => ToAddress -> TestEnv m C.Address
toAddress =
    either return $
    \(userIndex,addressIndex) ->
         do user <- getUser userIndex
            publicAddresses <- query' user U.GetPublicAddresses
            return . C.Address $ publicAddresses `indexModulo` addressIndex

toInputs :: WorkMode m => UserIndex -> FromAddresses -> TestEnv m Inputs
toInputs userIndex (getNonEmpty -> fromIndexes) = do
    user <- getUser userIndex
    allAddresses <- query' user U.GetAllAddresses
    publicAddresses <- query' user U.GetPublicAddresses
    addressesAmount <- mapM (U.getAmount user) allAddresses
    when (null publicAddresses) $
        throwM $ TestError "No public addresses in this user"
    return $
        nubBy ((==) `on` fst) .
        map (second $ filter (> 0)) .
        map (second C.coinsToList) .
        map
            (\(i,parts) ->
                  (i, applyPartsToSend parts $ addressesAmount `indexModulo` i)) $
        fromIndexes

getUser :: WorkMode m => UserIndex -> TestEnv m U.RSCoinUserState
getUser Nothing =
    view $ buser . state
getUser (Just index) = do
    mUser <- view $ users . to (`indexModuloMay` index)
    maybe (throwM $ TestError "No user in context") (return . view state) mUser
