{-# LANGUAGE FlexibleContexts #-}

-- | This module defines how to initialize RSCoin.

module Test.RSCoin.Full.Initialization
       ( mkTestContext, finishTest
       , bankUserAddressesCount, userAddressesCount
       ) where

import           Control.Exception         (assert)
import           Control.Lens              ((^.))
import           Control.Monad             (forM_, replicateM)
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
import           RSCoin.Timed              (Second, WorkMode, for, mcs, ms,
                                            myThreadId, upto, wait, work)
import qualified RSCoin.User               as U

import           Test.RSCoin.Full.Context  (BankInfo (..), MintetteInfo (..),
                                            MintetteNumber, Scenario (..),
                                            TestContext (..), TestEnv,
                                            UserInfo (..), UserNumber, port,
                                            publicKey, secretKey, state)
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
    logInfo testingLoggerName "Initializing system…"
    runMintettes minfos scen
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    mapM_ (addMintetteToBank binfo) minfos
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    runBank binfo
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    initBUser buinfo
    mapM_ initUser uinfos
    logInfo testingLoggerName "Successfully initialized system"
    return $ TestContext binfo minfos buinfo uinfos scen
  where
    mkMintette idx =
        MintetteInfo <$> liftIO keyGen <*> liftIO M.openMemState <*>
        pure (2300 + fromIntegral idx)
    bankKey = pure (bankSecretKey, derivePublicKey bankSecretKey)
    shortWait = wait $ for 10 ms

-- | Finish everything that's going on in TestEnv.
finishTest :: WorkMode m => TestEnv m ()
finishTest = return ()

-- | Number of addresses each casual user has in wallet (constant).
userAddressesCount :: Num a => a
userAddressesCount = 5

-- | Number of addresses each bank user has in wallet (constant).
bankUserAddressesCount :: Num a => a
bankUserAddressesCount = 6

runBank
    :: WorkMode m
    => BankInfo -> m ()
runBank b = do
    myTId <- myThreadId
    let periodLength = fromMaybe defaultPeriodDelta periodDelta
    work (upto 0 mcs) $
        B.runWorkerWithPeriod periodLength (b ^. secretKey) (b ^. state)
    work (upto 0 mcs) $ B.serve (b ^. state) myTId pure

runMintettes
    :: WorkMode m
    => [MintetteInfo] -> Scenario -> m ()
runMintettes mts scen = do
    case scen of
        DefaultScenario -> mapM_ TM.defaultMintetteInit mts
        (MalfunctioningMintettes d) -> do
            let (other,normal) = (take (partSize d) mts, drop (partSize d) mts)
            forM_ normal $ TM.defaultMintetteInit
            forM_ other $ TM.malfunctioningMintetteInit
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
