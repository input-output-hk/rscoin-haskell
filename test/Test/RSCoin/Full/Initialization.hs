-- | This module defines how to initialize RSCoin.

module Test.RSCoin.Full.Initialization
       ( InitAction (InitAction)
       ) where

import           Control.Exception         (assert)
import           Control.Lens              (view, (^.))
import           Control.Monad             (forM_)
import           Control.Monad.Trans       (MonadIO)
import           Data.Acid.Advanced        (update')
import           Data.List                 (genericLength)

import qualified RSCoin.Bank               as B
import           RSCoin.Core               (Mintette (..), bankSecretKey)
import           RSCoin.Timed              (WorkMode, for, mcs, upto, wait,
                                            work)
import qualified RSCoin.User               as U

import           Test.RSCoin.Full.Action   (Action (doAction))
import           Test.RSCoin.Full.Context  (MintetteInfo, Scenario (..),
                                            TestEnv, UserInfo, bank,
                                            bankUserAddressesCount, buser,
                                            lifetime, mintettes, port,
                                            publicKey, scenario, secretKey,
                                            state, userAddressesCount, users)
import qualified Test.RSCoin.Full.Mintette as TM

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
    U.initStateBank st (bankUserAddressesCount - 1) bankSecretKey

initUser :: WorkMode m => UserInfo -> TestEnv m ()
initUser user = U.initState (user ^. state) userAddressesCount Nothing
