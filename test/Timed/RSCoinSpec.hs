{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}

module Timed.RSCoinSpec 
       (
       )

import           Control.Exception     (Exception)
import           Control.Lens          (view, (^.), preview, ix, to)
import           Control.Monad         (forM, when)
import           Control.Monad.Catch   (throwM, catch)
import           Control.Monad.Trans   (MonadIO, liftIO)
import           Control.Monad.Reader  (runReaderT)
import           Data.Acid             (update, query)
import           Data.Default          (def)
import           Data.Int              (Int64)
import           Data.List             (nubBy)
import           Data.Function         (on)
import           Data.Maybe            (fromJust)
import           Data.Text             (Text, pack)
import           Data.Typeable         (Typeable)
import           System.Random         (mkStdGen)
import           Serokell.Util.Text    (formatSingle')

import           Test.QuickCheck       (Arbitrary (arbitrary), NonNegative (..),
                                        Gen, oneof, Positive (..),
                                        NonEmptyList (..), generate, frequency, vector)

import qualified RSCoin.Bank           as B
import qualified RSCoin.Mintette       as M
import qualified RSCoin.User           as U
import qualified RSCoin.Core.Arbitrary ()

import           RSCoin.Core           (initLogging, Severity(Info), Mintette(..),
                                        Address (..), logDebug, Coin (..),
                                        RSCoinError, logWarning)
import           RSCoin.Test           (WorkMode, runRealMode, runEmulationMode,
                                        upto, mcs, work, minute, wait, for, sec,
                                        interval, MicroSeconds, PureRpc, fork,
                                        invoke, at)
import           Timed.Context         (TestEnv, mkTestContext, state, port, 
                                        keys, publicKey, secretKey, MintetteInfo,
                                        bank, mintettes, lifetime, users, buser,
                                        UserInfo, bankSkPath)

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
type ToAddress = Either Address (UserIndex, ValidAddressIndex)
type FromAddresses = NonEmptyList (ValidAddressIndex, NonNegative Int)

type Inputs = [(Int, Int64)]

arbitraryAddress :: WorkMode m => ToAddress -> TestEnv m Address
arbitraryAddress =
    either return $
        \(userIndex, getNonNegative -> addressIndex) -> do
            user <- getUser userIndex
            publicAddresses <- liftIO $ query user U.GetPublicAddresses
            return . Address $ cycle publicAddresses !! addressIndex

arbitraryInputs :: WorkMode m => UserIndex -> FromAddresses -> TestEnv m Inputs
arbitraryInputs userIndex (getNonEmpty -> fromIndexes) = do
    return [(1, 50)]
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
    | UpdateBlockchain UserIndex
   -- TODO: we use dumping only for debug but we should cover all cases
   -- | Dump DumpAction
    deriving Show

instance Arbitrary UserAction where
    arbitrary =
        frequency [ (10, FormTransaction <$> arbitrary <*> arbitrary <*> arbitrary)
                  , (10, UpdateBlockchain <$> arbitrary)
                  ]

instance Action UserAction where
    doAction (FormTransaction userIndex' fromAddresses toAddress) = do
        -- always run in bank mode
        let userIndex = Nothing
        address <- arbitraryAddress toAddress
        inputs <- arbitraryInputs userIndex fromAddresses
        getUser userIndex >>= \s ->
            U.formTransaction s inputs address $
                Coin (sum $ map snd inputs)
    doAction (UpdateBlockchain userIndex) = do
        user <- getUser userIndex
        walletHeight <- liftIO $ query user U.GetLastBlockId
        lastBlockHeight <- pred <$> U.getBlockchainHeight
        mapM_ (U.updateToBlockHeight user) [walletHeight + 1 .. lastBlockHeight]

getUser :: WorkMode m => UserIndex -> TestEnv m U.RSCoinUserState
getUser Nothing =
    view $ buser . state
getUser (fromJust -> getNonNegative -> index) = do
    mState <- preview $ users . to cycle . ix index . state
    maybe (throwM $ TestError "No user in context") return mState

instance Arbitrary SomeAction where
    arbitrary = oneof [ SomeAction <$> (arbitrary :: Gen UserAction)
                      ]

-- TODO: maybe we should create also actions StartMintette, AddMintette, in terms of actions

main :: IO ()
main = do
    test 10 10 10

test :: Int -> Int -> Int -> IO ()
test mNum uNum actionNum = launchPure mNum uNum $ do
    actions <- liftIO $ generate (vector actionNum :: Gen [WaitAction UserAction])
    liftIO $ print actions
    mapM_ (\a -> doAction a `catch` handler) actions
  where
    handler (e :: RSCoinError) =
        logWarning $
            formatSingle' "Error occured while testing: {}" $ show e

launchPure :: Int -> Int -> TestEnv (PureRpc IO) () -> IO ()
launchPure mNum uNum = runEmulationMode (mkStdGen 9452) def . launch mNum uNum

launch :: WorkMode m => Int -> Int -> TestEnv m () -> m ()
launch mNum uNum test = do
    liftIO $ initLogging Info

    -- mNum mintettes, uNum users (excluding user in bank-mode), 
    -- emulation duration - 3 minutes
    (mkTestContext mNum uNum (interval 100 sec) >>= ) $ runReaderT $ do
        runBank
        mapM_ runMintette =<< view mintettes

        wait $ for 5 sec  -- ensure that bank & mintettes have initialized
 
        mapM_ addMintetteToBank =<< view mintettes
        initBUser
        mapM_ initUser =<< view users

        wait $ for 5 sec  -- ensure that users have initialized

        test
    

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
    liftIO $ update bankSt $ B.AddMintette addedMint mintPKey
 
initBUser :: WorkMode m => TestEnv m ()   
initBUser = do
    st <- view $ buser . state
    skPath <- bankSkPath
    U.initState st 5 (Just skPath)

initUser :: WorkMode m => UserInfo -> TestEnv m ()
initUser user = U.initState (user ^. state) 5 Nothing
