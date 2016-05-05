{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns              #-}

import           Control.Lens          (view, (^.), preview, ix, to)
import           Control.Monad.Trans   (MonadIO, liftIO)
import           Control.Monad.Reader  (runReaderT)
import           Data.Acid             (update, query)
import           Data.Int              (Int64)
import           Data.Default          (def)
import           Data.Maybe            (fromJust)
import           System.Random         (mkStdGen)

import           Test.QuickCheck       (Arbitrary (arbitrary), NonNegative (..),
                                        Gen, oneof, Positive (..), NonEmptyList (..))

import qualified  RSCoin.Bank          as B
import qualified  RSCoin.Mintette      as M
import qualified  RSCoin.User          as U
import qualified  Actions              as U
import qualified  UserOptions          as U
import            RSCoin.Core          (initLogging, Severity(Info), Mintette(..),
                                        Address)
import           RSCoin.Test           (WorkMode, runRealMode, runEmulationMode,
                                        upto, mcs, work, minute, wait, for, sec,
                                        interval, MicroSeconds)
import           RSCoin.Core.Arbitrary ()
import           Context               (TestEnv, mkTestContext, state, port, 
                                        keys, publicKey, secretKey, MintetteInfo,
                                        bank, mintettes, lifetime, users, buser,
                                        UserInfo, bankSkPath)

class Action a where
    doAction :: WorkMode m => a -> TestEnv m ()

data SomeAction = forall a . Action a => SomeAction a

data EmptyAction = EmptyAction
    deriving Show

instance Action EmptyAction where
    doAction _ = pure ()

instance Arbitrary EmptyAction where
    arbitrary = pure EmptyAction

data WaitAction = WaitAction (NonNegative MicroSeconds)
    deriving Show

instance Action WaitAction where
    doAction (WaitAction (getNonNegative -> time)) =
        wait $ for time mcs

instance Arbitrary WaitAction where
    arbitrary = WaitAction <$> arbitrary

-- | Nothing represents bank user, otherwise user is selected according
-- to index in the list
type UserIndex = Maybe (NonNegative Int)

type InvalidAddress = Maybe Address
type ValidAddressIndex = NonNegative Int
type ToAddress = (UserIndex, InvalidAddress, ValidAddressIndex)
type FromAddresses = NonEmptyList (ValidAddressIndex, Positive Int64)


-- data DumpAction

data UserAction
    = ListAddresses UserIndex
    | FormTransaction UserIndex FromAddresses ToAddress
    | UpdateBlockchain UserIndex
   -- TODO: we use dumping only for debug but we should cover all cases
   -- | Dump DumpAction
    deriving Show

instance Arbitrary UserAction where
    arbitrary =
        oneof [ ListAddresses <$> arbitrary
              , FormTransaction <$> arbitrary <*> arbitrary <*> arbitrary
              , UpdateBlockchain <$> arbitrary
              ]

instance Action UserAction where
    doAction (ListAddresses userIndex) =
        runUserAction userIndex U.ListAddresses
    doAction (FormTransaction userIndex fromAddresses toAddress) = do
        publicAddresses <- getUser userIndex >>= liftIO . flip query U.GetPublicAddresses

        undefined
    doAction (UpdateBlockchain userIndex) =
        runUserAction userIndex U.UpdateBlockchain

runUserAction :: WorkMode m => UserIndex -> U.UserCommand -> TestEnv m ()
runUserAction user command =
    getUser user >>= flip U.proceedCommand command

getUser :: WorkMode m => UserIndex -> TestEnv m U.RSCoinUserState
getUser Nothing =
    view $ buser . state
getUser (fromJust -> getNonNegative -> index) =
    fmap fromJust . preview $ users . to cycle . ix index . state

instance Arbitrary SomeAction where
    arbitrary = oneof [ SomeAction <$> (arbitrary :: Gen EmptyAction) -- I am not sure does this makes sense when we have WaitAction
                      , SomeAction <$> (arbitrary :: Gen WaitAction)
                      , SomeAction <$> (arbitrary :: Gen UserAction)
                      ]

-- TODO: maybe we should create also actions StartMintette, AddMintette, in terms of actions

main :: IO ()
main = return ()

launchPure :: Int -> Int -> IO ()
launchPure mNum uNum = runEmulationMode (mkStdGen 9452) def $ launch mNum uNum

launch :: WorkMode m => Int -> Int -> m ()
launch mNum uNum = do
    liftIO $ initLogging Info

    -- mNum mintettes, uNum users (excluding user in bank-mode), 
    -- emulation duration - 3 minutes
    (mkTestContext mNum uNum (interval 3 minute) >>= ) $ runReaderT $ do
        runBank
        mapM_ runMintette =<< view mintettes

        wait $ for 5 sec  -- ensure that bank & mintettes have initialized
 
        mapM_ addMintetteToBank =<< view mintettes
        initBUser
        mapM_ initUser =<< view users
    

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
