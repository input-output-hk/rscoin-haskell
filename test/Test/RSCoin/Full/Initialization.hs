{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

-- | This module defines how to initialize RSCoin.

module Test.RSCoin.Full.Initialization
       ( mkTestContext, finishTest
       , bankUserAddressesCount, userAddressesCount
       ) where

import           Control.Concurrent.MVar    (MVar, newEmptyMVar, tryPutMVar)
import           Control.Exception          (assert)
import           Control.Lens               (view, (^.))
import           Control.Monad              (replicateM)
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.Trans        (MonadIO (liftIO))
import           Data.Acid.Advanced         (update')
import           Data.List                  (genericLength)
import qualified Data.Map                   as M
import           Formatting                 (build, sformat, (%))
import           Test.QuickCheck            (NonEmptyList (..))

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Mintette (..), bankSecretKey,
                                             defaultPeriodDelta,
                                             derivePublicKey, keyGen, logDebug,
                                             logInfo, testingLoggerName)
import qualified RSCoin.Mintette            as M
import           RSCoin.Timed               (Second, WorkMode, for, ms,
                                             myThreadId, sec, wait,
                                             workWhileMVarEmpty)
import qualified RSCoin.User                as U

import           Test.RSCoin.Full.Action    (Coloring (Coloring),
                                             PartsToSend (PartsToSend),
                                             UserAction (SubmitTransaction),
                                             doAction)
import           Test.RSCoin.Full.Constants (bankUserAddressesCount, maxColor,
                                             minColor, userAddressesCount)
import           Test.RSCoin.Full.Context   (BankInfo (..), MintetteInfo (..),
                                             MintetteNumber, Scenario (..),
                                             TestContext (..), TestEnv,
                                             UserInfo (..), UserNumber,
                                             isActive, port, publicKey,
                                             secretKey, state, users)
import qualified Test.RSCoin.Full.Mintette  as TM

periodDelta :: Second
periodDelta = defaultPeriodDelta

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
    let ctx = TestContext binfo minfos buinfo uinfos scen isActiveVar
    sendInitialCoins ctx
    wait $ for periodDelta sec
    logInfo testingLoggerName "Successfully initialized system"
    return ctx
  where
    mkMintette idx =
        MintetteInfo <$> liftIO keyGen <*> liftIO M.openMemState <*>
        pure (2300 + fromIntegral idx)
    bankKey = pure (bankSecretKey, derivePublicKey bankSecretKey)
    shortWait = wait $ for 10 ms

-- | Finish everything that's going on in TestEnv.
finishTest :: WorkMode m => TestEnv m ()
finishTest = () <$ (liftIO . flip tryPutMVar () =<< view isActive)

runBank
    :: WorkMode m
    => MVar () -> BankInfo -> m ()
runBank v b = do
    myTId <- myThreadId
    workWhileMVarEmpty v $
        B.runWorkerWithPeriod periodDelta (b ^. secretKey) (b ^. state)
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

sendInitialCoins
    :: WorkMode m
    => TestContext -> m ()
sendInitialCoins ctx = do
    genesisIdxs <- mapM U.genesisAddressIndex (map _userState $ _users ctx)
    runReaderT (mapM_ doAction $ actions genesisIdxs) ctx
  where
    usersNum = length (ctx ^. users)
    addressesCount = userAddressesCount * usersNum + bankUserAddressesCount
    partToSend = recip $ realToFrac addressesCount
    partsToSend = PartsToSend $ M.singleton 0 partToSend
    outputs =
        [Right (Nothing, addrIdx) | addrIdx <-
                                       [0 .. bankUserAddressesCount - 1]] ++
        [Right (Just (fromIntegral usrIdx), addrIdx) | usrIdx <-
                                                          [0 .. usersNum - 1]
                                                     , addrIdx <-
                                                           [0 .. userAddressesCount -
                                                                 1]]
    allColors = [minColor .. maxColor]
    nonZeroColors = filter (/= 0) allColors
    coloring =
        Just . Coloring . M.fromList . map (, recip (genericLength allColors)) $
        nonZeroColors
    actions genesisList =
        map
            (\o ->
                  SubmitTransaction
                      Nothing
                      (NonEmpty $ zipWith help genesisList (repeat partsToSend))
                      o
                      coloring)
            outputs
    help (Just genAdrInd) part = (genAdrInd, part)
    help _ _ = error "[FATAL] RSCoin is broken: genesisAddressIndex return Nothing for bank user"
