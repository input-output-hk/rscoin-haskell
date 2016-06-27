{-# LANGUAGE FlexibleContexts #-}

-- | This module defines how to initialize RSCoin.

module Test.RSCoin.Full.Initialization
       ( mkTestContext, finishTest
       , bankUserAddressesCount, userAddressesCount
       ) where

import           Control.Concurrent.MVar   (MVar, isEmptyMVar, newEmptyMVar,
                                            tryPutMVar)
import           Control.Exception         (assert)
import           Control.Lens              (view, (^.))
import           Control.Monad             (replicateM)
import           Control.Monad.Trans       (MonadIO (liftIO))
import           Data.Acid.Advanced        (update')
import           Data.List                 (genericLength)
import           Data.Maybe                (fromMaybe)
import           Formatting                (build, sformat, (%))

import qualified RSCoin.Bank               as B
import           RSCoin.Core               (Mintette (..), bankSecretKey,
                                            defaultPeriodDelta, derivePublicKey,
                                            keyGen, logDebug, logInfo,
                                            testingLoggerName)
import qualified RSCoin.Mintette           as M
import           RSCoin.Timed              (Second, WorkMode, for, ms,
                                            myThreadId, wait, workWhile)
import qualified RSCoin.User               as U

import           Test.RSCoin.Full.Context  (BankInfo (..), MintetteInfo (..),
                                            MintetteNumber, Scenario (..),
                                            TestContext (..), TestEnv,
                                            UserInfo (..), UserNumber, isActive,
                                            port, publicKey, secretKey, state)
import qualified Test.RSCoin.Full.Mintette as TM

periodDelta :: Maybe Second
periodDelta = Nothing

-- | Start all servers/workers and create TestContext.
mkTestContext
    :: WorkMode m
    => MintetteNumber -> UserNumber -> Scenario -> m TestContext
mkTestContext mNum uNum scen = do
    binfo <- BankInfo <$> bankKey <*> liftIO B.openMemState
    minfos <- mapM mkMintette [0 .. mNum - 1]
    buinfo <- UserInfo <$> liftIO U.openMemState
    uinfos <-
        replicateM (fromIntegral uNum) $ UserInfo <$> liftIO U.openMemState
    isActiveVar <- liftIO newEmptyMVar
    logInfo testingLoggerName "Initializing system…"
    runMintettes isActiveVar minfos scen
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    mapM_ (addMintetteToBank binfo) minfos
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    runBank isActiveVar binfo
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    initBUser buinfo
    mapM_ initUser uinfos
    logInfo testingLoggerName "Successfully initialized system"
    return $ TestContext binfo minfos buinfo uinfos scen isActiveVar
  where
    mkMintette idx =
        MintetteInfo <$> liftIO keyGen <*> liftIO M.openMemState <*>
        pure (2300 + fromIntegral idx)
    bankKey = pure (bankSecretKey, derivePublicKey bankSecretKey)
    shortWait = wait $ for 10 ms

-- | Finish everything that's going on in TestEnv.
finishTest :: WorkMode m => TestEnv m ()
finishTest = () <$ (liftIO . flip tryPutMVar () =<< view isActive)

-- | Number of addresses each casual user has in wallet (constant).
userAddressesCount :: Num a => a
userAddressesCount = 5

-- | Number of addresses each bank user has in wallet (constant).
bankUserAddressesCount :: Num a => a
bankUserAddressesCount = 6

workWhileMVarEmpty
    :: WorkMode m
    => MVar a -> m () -> m ()
workWhileMVarEmpty v = workWhile (liftIO . isEmptyMVar $ v)

runBank
    :: WorkMode m
    => MVar () -> BankInfo -> m ()
runBank v b = do
    myTId <- myThreadId
    let periodLength = fromMaybe defaultPeriodDelta periodDelta
    workWhileMVarEmpty v $
        B.runWorkerWithPeriod periodLength (b ^. secretKey) (b ^. state)
    workWhileMVarEmpty v $ B.serve (b ^. state) myTId pure

runMintettes
    :: WorkMode m
    => MVar () -> [MintetteInfo] -> Scenario -> m ()
runMintettes v mts scen = do
    case scen of
        DefaultScenario -> mapM_ (TM.defaultMintetteInit v) mts
        (MalfunctioningMintettes d) -> do
            let (other,normal) = (take (partSize d) mts, drop (partSize d) mts)
            mapM_ (TM.defaultMintetteInit v) normal
            mapM_ (TM.malfunctioningMintetteInit v) other
        _ -> error "Test.Action.runMintettes not implemented"
  where
    partSize :: Double -> Int
    partSize d = assert (d >= 0 && d <= 1) $ floor $ genericLength mts * d

addMintetteToBank
    :: MonadIO m
    => BankInfo -> MintetteInfo -> m ()
addMintetteToBank b mintette = do
    let addedMint = Mintette "127.0.0.1" (mintette ^. port)
        mintPKey  = mintette ^. publicKey
        bankSt = b ^. state
    logDebug testingLoggerName $ sformat ("Adding mintette " % build) addedMint
    update' bankSt $ B.AddMintette addedMint mintPKey
    logDebug testingLoggerName $ sformat ("Added mintette " % build) addedMint

initBUser
    :: WorkMode m
    => UserInfo -> m ()
initBUser bu =
    U.initStateBank (bu ^. state) (bankUserAddressesCount - 1) bankSecretKey

initUser
    :: WorkMode m
    => UserInfo -> m ()
initUser user = U.initState (user ^. state) userAddressesCount Nothing
