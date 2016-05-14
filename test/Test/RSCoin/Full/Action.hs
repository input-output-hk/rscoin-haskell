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
       , UserIndex
       , getUser
       ) where

import           Control.Exception         (assert)
import           Control.Lens              (to, view, (^.))
import           Control.Monad             (forM_, when)
import           Control.Monad.Catch       (throwM)
import           Control.Monad.Trans       (MonadIO)
import           Data.Acid.Advanced        (query', update')
import           Data.Bifunctor            (bimap)
import           Data.Function             (on)
import           Data.List                 (genericLength, nubBy)
import           Test.QuickCheck           (NonEmptyList (..), NonNegative (..))

import           Serokell.Util             (indexModulo, indexModuloMay)

import qualified RSCoin.Bank               as B
import           RSCoin.Core               (Address (..), Coin (..),
                                            Mintette (..), bankSecretKey)
import           RSCoin.Timed              (Second, WorkMode, for, invoke, mcs,
                                            sec, upto, work, wait)
import qualified RSCoin.User               as U

import           Test.RSCoin.Full.Context  (MintetteInfo, Scenario (..),
                                            TestEnv, UserInfo, bank, buser,
                                            lifetime, mintettes, port,
                                            publicKey, scenario, secretKey,
                                            state, users)
import           Test.RSCoin.Full.Error    (TestError (TestError))
import qualified Test.RSCoin.Full.Mintette as TM

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

data InitAction = InitAction
    deriving (Show)

instance Action InitAction where
    doAction InitAction = do
        runBank
        scen <- view scenario
        mint <- view mintettes
        runMintettes mint scen
        mapM_ addMintetteToBank =<< view mintettes
        wait $ for 1 mcs -- this is necessary
        initBUser
        mapM_ initUser =<< view users

runBank :: WorkMode m => TestEnv m ()
runBank = do
    b <- view bank
    l <- view lifetime
    work (upto l mcs) $ B.runWorker (b ^. secretKey) (b ^. state)
    work (upto l mcs) $ B.serve (b ^. state)

runMintettes :: WorkMode m => [MintetteInfo] -> Scenario -> TestEnv m ()
runMintettes ms scen = do
    l <- view lifetime
    case scen of
        DefaultScenario -> mapM_ (TM.defaultMintetteInit l) ms
        (MalfunctioningMintettes d) -> do
            let (other,normal) = (take (partSize d) ms, drop (partSize d) ms)
            forM_ normal $ TM.defaultMintetteInit l
            forM_ other $ TM.malfunctioningMintetteInit l
        _ -> error "Test.Action.runMintettes not implemented"
  where
    partSize :: Double -> Int
    partSize d = assert (d >= 0 && d <= 1) $ floor $ genericLength ms * d

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
type UserIndex = Maybe Word

-- | Address will be either some arbitrary address or some user address
type ToAddress = Either Address (UserIndex, Word)

type FromAddresses = NonEmptyList (Word, Word)

type Inputs = [(Word, Coin)]

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
                 sum $ map snd inputs
    doAction (ListAddresses userIndex) =
        runUserAction userIndex U.ListAddresses
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
    -- TODO: for now we are sending all coins. It would be good to send some amount of coins that we have
    return $
        nubBy ((==) `on` fst) .
        filter ((> 0) . snd) .
        addAtLeastOneAddress addressesAmount .
        map (bimap succ $ indexModulo addressesAmount) $
        fromIndexes
  where
    addAtLeastOneAddress addressesAmount =
        ((1, addressesAmount !! 0) :)

getUser :: WorkMode m => UserIndex -> TestEnv m U.RSCoinUserState
getUser Nothing =
    view $ buser . state
getUser (Just index) = do
    mUser <- view $ users . to (`indexModuloMay` index)
    maybe (throwM $ TestError "No user in context") (return . view state) mUser

runUserAction :: WorkMode m => UserIndex -> U.UserCommand -> TestEnv m ()
runUserAction userIndex command =
    getUser userIndex >>= flip U.proceedCommand command
