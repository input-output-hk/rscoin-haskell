{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns              #-}

import           Control.Lens         (view, (^.))
import           Control.Monad.Trans  (MonadIO, liftIO)
import           Control.Monad.Reader (runReaderT)
import           Data.Acid            (update)
import           Data.Default         (def)
import           System.Random        (mkStdGen)

import           Test.QuickCheck      (Arbitrary (arbitrary), NonNegative (..),
                                       Gen, oneof)

import           RSCoin.Bank          as B
import           RSCoin.Mintette      as M
import           RSCoin.User          as U
import           RSCoin.Core          (initLogging, Severity(Info), Mintette(..))
import           RSCoin.Test          (WorkMode, runRealMode, runEmulationMode,
                                       upto, mcs, work, minute, wait, for, sec,
                                       interval, MicroSeconds)
import           Context              (TestEnv, mkTestContext, state, port, 
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
    doAction (WaitAction (getNonNegative -> time)) = wait $ for time mcs

instance Arbitrary WaitAction where
    arbitrary = WaitAction <$> arbitrary

instance Arbitrary SomeAction where
    arbitrary = oneof [ SomeAction <$> (arbitrary :: Gen EmptyAction) -- I am not sure does this makes sense when we have WaitAction
                      , SomeAction <$> (arbitrary :: Gen WaitAction)
                      ]

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
