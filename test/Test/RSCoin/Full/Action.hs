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
       , InitAction (..)
       , UserAction (..)
       ) where

import           Control.Lens             (ix, preview, to, view, (^.))
import           Control.Monad.Catch      (throwM)
import           Control.Monad.Trans      (MonadIO)
import           Data.Acid.Advanced       (update')
import           Data.Acid.Advanced       (query')
import           Data.Int                 (Int64)
import           Data.Maybe               (fromJust)
import           Test.QuickCheck          (NonEmptyList (..), NonNegative (..))

import qualified RSCoin.Bank              as B
import           RSCoin.Core              (Address (..), Coin (..),
                                           Mintette (..), bankSecretKey)
import qualified RSCoin.Mintette          as M
import           RSCoin.Timed             (Second, WorkMode, for, invoke, mcs,
                                           sec, upto, wait, work)
import qualified RSCoin.User              as U

import           Test.RSCoin.Full.Context (MintetteInfo, TestEnv, UserInfo,
                                           bank, buser, lifetime, mintettes,
                                           port, publicKey, secretKey, state,
                                           users)
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

data InitAction =
    InitAction
    deriving (Show)

instance Action InitAction where
    doAction InitAction = do
        mapM_ runMintette =<< view mintettes
        mapM_ addMintetteToBank =<< view mintettes
        runBank
        wait $ for 5 sec  -- ensure that bank and mintettes are initialized
        initBUser
        mapM_ initUser =<< view users
        wait $ for 5 sec  -- ensure that users are initialized

runBank :: WorkMode m => TestEnv m ()
runBank = do
    b <- view bank
    l <- view lifetime
    work (upto l mcs) $ B.runWorker (b ^. secretKey) (b ^. state)
    work (upto l mcs) $ B.serve (b ^. state)

runMintette :: WorkMode m => MintetteInfo -> TestEnv m ()
runMintette m = do
    l <- view lifetime
    work (upto l mcs) $
        M.serve <$> view port <*> view state <*> view secretKey $ m
    work (upto l mcs) $
        M.runWorker <$> view secretKey <*> view state $ m

addMintetteToBank :: MonadIO m => MintetteInfo -> TestEnv m ()
addMintetteToBank mintette = do
    let addedMint = Mintette "127.0.0.1" (mintette ^. port)
        mintPKey  = mintette ^. publicKey
    bankSt <- view $ bank . state
    update' bankSt $ B.AddMintette addedMint mintPKey

initBUser :: WorkMode m => TestEnv m ()
initBUser = do
    st <- view $ buser . state
    U.initStateBank st 5 bankSecretKey

initUser :: WorkMode m => UserInfo -> TestEnv m ()
initUser user = U.initState (user ^. state) 5 Nothing

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
