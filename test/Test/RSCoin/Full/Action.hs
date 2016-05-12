{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Actions affecting global context.

module Test.RSCoin.Full.Action
       ( Action (..)
       , SomeAction (..)
       , EmptyAction (..)
       , WaitAction (..)
       , WaitSomeAction
       , UserAction (..)
       ) where

import           Control.Lens             (ix, preview, to, view)
import           Control.Monad.Catch      (throwM)
import           Data.Acid.Advanced       (query')
import           Data.Int                 (Int64)
import           Data.Maybe               (fromJust)
import           Test.QuickCheck          (NonEmptyList (..), NonNegative (..))

import           RSCoin.Core              (Address (..), Coin (..))
import           RSCoin.Timed             (Second, WorkMode, for, invoke, sec)
import qualified RSCoin.User              as U

import           Test.RSCoin.Full.Context (TestEnv, buser, state, users)
import           Test.RSCoin.Full.Error   (TestError (TestError))

class Action a where
    doAction :: WorkMode m => a -> TestEnv m ()

data SomeAction = forall a . (Action a, Show a) => SomeAction a

instance Show SomeAction where
    show (SomeAction a) = show a

instance Action SomeAction where
    doAction (SomeAction a) = doAction a

data EmptyAction = EmptyAction
    deriving Show

instance Action EmptyAction where
    doAction _ = pure ()

data WaitAction a = WaitAction (NonNegative Second) a
    deriving Show

type WaitSomeAction = WaitAction SomeAction

instance Action a => Action (WaitAction a) where
    doAction (WaitAction (getNonNegative -> time) action) =
        invoke (for time sec) $ doAction action

-- | Nothing represents bank user, otherwise user is selected according
-- to index in the list
type UserIndex = Maybe (NonNegative Int)

type ValidAddressIndex = NonNegative Int

-- | Address will be either some arbitrary address or some user address
type ToAddress = Either Address (UserIndex, ValidAddressIndex)

type FromAddresses = NonEmptyList (ValidAddressIndex, NonNegative Int)

type Inputs = [(Int, Int64)]

-- data DumpAction

data UserAction
    = FormTransaction UserIndex FromAddresses ToAddress
    | ListAddresses UserIndex
    | UpdateBlockchain UserIndex
   -- TODO: we use dumping only for debug but we should cover all cases
   -- | Dump DumpAction
    deriving Show

instance Action UserAction where
    doAction (FormTransaction userIndex fromAddresses toAddr) = do
        address <- toAddress toAddr
        inputs <- toInputs userIndex fromAddresses
        getUser userIndex >>=
            \s ->
                 U.formTransaction s inputs address $
                 Coin (sum $ map snd inputs)
    doAction (ListAddresses userIndex) = do
        runUserAction userIndex U.ListAddresses
    doAction (UpdateBlockchain userIndex) = do
        runUserAction userIndex U.UpdateBlockchain

toAddress :: WorkMode m => ToAddress -> TestEnv m Address
toAddress =
    either return $
        \(userIndex, getNonNegative -> addressIndex) -> do
            user <- getUser userIndex
            publicAddresses <- query' user U.GetPublicAddresses
            return . Address $ cycle publicAddresses !! addressIndex

toInputs :: WorkMode m => UserIndex -> FromAddresses -> TestEnv m Inputs
toInputs _ _ =
    return [(1, 50)]
-- arbitraryInputs userIndex (getNonEmpty -> fromIndexes) = do
--    user <- getUser userIndex
--    allAddresses <- liftIO $ query user U.GetAllAddresses
--    publicAddresses <- liftIO $ query user U.GetPublicAddresses
--    addressesAmount <- mapM (U.getAmount user) allAddresses
--    when (null publicAddresses) $
--        throwM $ TestError "No public addresses in this user"
--    -- TODO: for now we are sending all coins. It would be good to send some amount of coins that we have
--    return $ nubBy ((==) `on` fst)
--        $ filter ((> 0) . snd)
--        $ addAtLeastOneAddress addressesAmount
--        $ map (\(a, b) -> (a + 1, getCoin $ addressesAmount !! a))
--        $ map (\(getNonNegative -> a, getNonNegative -> b) -> (a `mod` length publicAddresses, b)) fromIndexes
--  where
--    addAtLeastOneAddress addressesAmount = ((1, getCoin $ addressesAmount !! 0):)

getUser :: WorkMode m => UserIndex -> TestEnv m U.RSCoinUserState
getUser Nothing =
    view $ buser . state
getUser (getNonNegative . fromJust -> index) = do
    mState <-
        preview $
        users . to cycle . ix index . state
    maybe (throwM $ TestError "No user in context") return mState

runUserAction :: WorkMode m => UserIndex -> U.UserCommand -> TestEnv m ()
runUserAction userIndex command =
    getUser userIndex >>= flip U.proceedCommand command
