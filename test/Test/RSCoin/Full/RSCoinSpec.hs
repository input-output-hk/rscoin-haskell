{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}

module Test.RSCoin.Full.RSCoinSpec
       ( spec
       ) where

import           Control.Concurrent.MVar    (MVar, newEmptyMVar, putMVar,
                                             readMVar)
import           Control.Exception          (Exception)
import           Control.Lens               (ix, preview, to, view, (^.))
import           Control.Monad.Catch        (throwM)
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.Acid.Advanced         (query', update')
import           Data.Int                   (Int64)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           Test.Hspec                 (Spec)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen,
                                             NonEmptyList (..),
                                             NonNegative (..),
                                             Property, frequency,
                                             oneof)
import           Test.QuickCheck.Monadic    (assert, monadicIO)

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Address (..), Coin (..),
                                             Mintette (..))
import qualified RSCoin.Mintette            as M
import           RSCoin.Timed               (MicroSeconds, WorkMode,
                                             for, invoke, mcs, minute,
                                             sec, upto, wait, work)
import qualified RSCoin.User                as U

import           Test.RSCoin.Full.Context   (MintetteInfo, TestContext, TestEnv,
                                             UserInfo, bank, bankSkPath, buser,
                                             lifetime, mintettes,
                                             mkTestContext, port, publicKey,
                                             secretKey, state, users)
import           Test.RSCoin.Core.Arbitrary ()

spec :: Spec
spec = return ()

data TestError
    = TestError Text
    deriving (Show, Typeable, Eq)

instance Exception TestError

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

instance Arbitrary EmptyAction where
    arbitrary = pure EmptyAction

data WaitAction a = WaitAction (NonNegative MicroSeconds) a
    deriving Show

type WaitSomeAction = WaitAction SomeAction

instance Action a => Action (WaitAction a) where
    doAction (WaitAction (getNonNegative -> time) action) =
        invoke (for time sec) $ doAction action

instance Arbitrary a => Arbitrary (WaitAction a) where
    arbitrary = WaitAction <$> arbitrary <*> arbitrary

-- | Nothing represents bank user, otherwise user is selected according
-- to index in the list
type UserIndex = Maybe (NonNegative Int)

type ValidAddressIndex = NonNegative Int

-- | Address will be either some arbitrary address or some user address
type ToAddress = Either Address (UserIndex, ValidAddressIndex)

type FromAddresses = NonEmptyList (ValidAddressIndex, NonNegative Int)

type Inputs = [(Int, Int64)]

arbitraryAddress :: WorkMode m => ToAddress -> TestEnv m Address
arbitraryAddress =
    either return $
        \(userIndex, getNonNegative -> addressIndex) -> do
            user <- getUser userIndex
            publicAddresses <- query' user U.GetPublicAddresses
            return . Address $ cycle publicAddresses !! addressIndex

arbitraryInputs :: WorkMode m => UserIndex -> FromAddresses -> TestEnv m Inputs
arbitraryInputs _ _ =
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

-- data DumpAction

data UserAction
    = FormTransaction UserIndex FromAddresses ToAddress
    | ListAddresses UserIndex
    | UpdateBlockchain UserIndex
   -- TODO: we use dumping only for debug but we should cover all cases
   -- | Dump DumpAction
    deriving Show

instance Arbitrary UserAction where
    arbitrary =
        frequency [ (10, FormTransaction <$> arbitrary <*> arbitrary <*> arbitrary)
                  , (1, ListAddresses <$> arbitrary)
                  , (10, UpdateBlockchain <$> arbitrary)
                  ]

instance Action UserAction where
    doAction (FormTransaction userIndex fromAddresses toAddress) = do
        address <- arbitraryAddress toAddress
        inputs <- arbitraryInputs userIndex fromAddresses
        getUser userIndex >>= \s ->
            U.formTransaction s inputs address $
                Coin (sum $ map snd inputs)
    doAction (ListAddresses userIndex) = do
        runUserAction userIndex U.ListAddresses
    doAction (UpdateBlockchain userIndex) = do
        runUserAction userIndex U.UpdateBlockchain

runUserAction :: WorkMode m => UserIndex -> U.UserCommand -> TestEnv m ()
runUserAction userIndex command =
    getUser userIndex >>= flip U.proceedCommand command

getUser :: WorkMode m => UserIndex -> TestEnv m U.RSCoinUserState
getUser Nothing =
    view $ buser . state
getUser (fromJust -> getNonNegative -> index) = do
    mState <- preview $ users . to cycle . ix index . state
    maybe (throwM $ TestError "No user in context") return mState

-- TODO: maybe we should create also StartMintette, AddMintette, in terms of actions
instance Arbitrary SomeAction where
    arbitrary = oneof [ SomeAction <$> (arbitrary :: Gen UserAction)
                      ]

newtype RSCoinState m =
    RSCoinState { stateContext :: m TestContext }

instance WorkMode m => Arbitrary (RSCoinState m) where
    arbitrary = do
        actions :: [WaitSomeAction] <- arbitrary
        let actionsRunningTime = sum $ map (\(WaitAction t _) -> getNonNegative t) actions
            safeRunningTime = actionsRunningTime + (minute 1)
        mNum <- arbitrary
        uNum <- arbitrary
        return $ RSCoinState $ (mkTestContext mNum uNum safeRunningTime >>=) $ runReaderT $ do
            runBank
            mapM_ runMintette =<< view mintettes

            wait $ for 5 sec  -- ensure that bank & mintettes have initialized

            mapM_ addMintetteToBank =<< view mintettes
            initBUser
            mapM_ initUser =<< view users

            wait $ for 5 sec  -- ensure that users have initialized

            mapM_ doAction actions

            wait $ for safeRunningTime sec -- wait for all actions to finish

            ask

-- launchPure :: Int -> Int -> TestEnv (PureRpc IO) () -> IO ()
-- launchPure mNum uNum = runEmulationMode (mkStdGen 9452) def . launch mNum uNum

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
    skPath <- bankSkPath
    U.initState st 5 (Just skPath)

initUser :: WorkMode m => UserInfo -> TestEnv m ()
initUser user = U.initState (user ^. state) 5 Nothing

runAnotherAction :: WorkMode m => RSCoinState m -> SomeAction -> RSCoinState m
runAnotherAction (stateContext -> context) action =
    RSCoinState $ context >>= runReaderT (doAction action >> ask)

-- TODO: this is a workaround until exception handling is fixed (waiting until @martoon merges).
-- We cant pull out value from PureRpc so one solution is to use create something like PureRpc (State Bool) and use that for testing. This solution is used in MonadRpcSpec module. Because PureRpc is MonadIO here, I am instead reusing MVar. Possibly better approach would be to throw some SpecialTestException and catch that exception outside. If exception is caught then test failed.
monadicIOProp :: WorkMode m => ReaderT (MVar Bool) m () -> Property
monadicIOProp action = monadicIO $ do
    var <- liftIO newEmptyMVar
    -- FIXME: run action >> putMVar True
    res <- liftIO $ readMVar var
    assert res

assertProp :: WorkMode m => Bool -> ReaderT (MVar Bool) m ()
assertProp True  = pure ()
assertProp False = ask >>= liftIO . flip putMVar False

somePropertyX :: WorkMode m => RSCoinState m -> Property
somePropertyX state = monadicIO $
    -- assert $ 1 == 1
    return ()

somePropertyAfterAction :: WorkMode m => RSCoinState m -> SomeAction -> Property
somePropertyAfterAction state action = monadicIO $
    -- runAnotherAction state action
    return ()
