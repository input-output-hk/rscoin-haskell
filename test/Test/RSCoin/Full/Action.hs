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
       , getUser
       ) where

import           Control.Lens             (to, view)
import           Control.Monad            (unless, when)
import           Control.Monad.Catch      (throwM)
import           Data.Acid.Advanced       (query')
import           Data.Function            (on)
import           Data.List                (genericIndex, genericLength, nubBy)
import           Test.QuickCheck          (NonEmptyList (..), NonNegative (..))

import           Serokell.Util            (indexModulo, indexModuloMay)

import           RSCoin.Core              (Address (..), Coin (..))
import           RSCoin.Timed             (Second, WorkMode, for, invoke, sec)
import qualified RSCoin.User              as U

import           Test.RSCoin.Full.Context (TestEnv, buser, publicKey, secretKey,
                                           state, users)
import           Test.RSCoin.Full.Error   (TestError (TestError))

class Action a where
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
type ToAddress = Either Address (UserIndex, Word)

type FromAddresses = NonEmptyList Word

type Inputs = [(Word, Coin)]

data UserAction
    = FormTransaction UserIndex FromAddresses ToAddress
    | UpdateBlockchain UserIndex
    deriving Show

instance Action UserAction where
    doAction (FormTransaction userIndex fromAddresses toAddr) = do
        address <- toAddress toAddr
        inputs <- toInputs userIndex fromAddresses
        user <- getUser userIndex
        unless (null inputs) $
            U.formTransaction user inputs address $ sum $ map snd inputs
    doAction (UpdateBlockchain userIndex) =
        runUserAction userIndex U.UpdateBlockchain

toAddress :: WorkMode m => ToAddress -> TestEnv m Address
toAddress =
    either return $
        \(userIndex, addressIndex) -> do
            user <- getUser userIndex
            publicAddresses <- query' user U.GetPublicAddresses
            return . Address $ publicAddresses `indexModulo` addressIndex

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
        filter ((> 0) . snd) .
        map
            (\i ->
                  (succ i, addressesAmount `genericIndex` i)) .
        map (`mod` genericLength publicAddresses) $
        fromIndexes

getUser :: WorkMode m => UserIndex -> TestEnv m U.RSCoinUserState
getUser Nothing =
    view $ buser . state
getUser (Just index) = do
    mUser <- view $ users . to (`indexModuloMay` index)
    maybe (throwM $ TestError "No user in context") (return . view state) mUser

runUserAction :: WorkMode m => UserIndex -> U.UserCommand -> TestEnv m ()
runUserAction userIndex command =
    getUser userIndex >>= flip U.proceedCommand command
