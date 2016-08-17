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
import qualified Data.IntMap.Strict         as M
import           Data.IORef                 (newIORef)
import           Data.List                  (genericLength)
import           Data.Maybe                 (fromMaybe)
import           Formatting                 (build, sformat, (%))
import           Test.QuickCheck            (NonEmptyList (..))

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Color (..), Mintette (..),
                                             SecretKey, WithNamedLogger,
                                             defaultPeriodDelta,
                                             derivePublicKey, keyGen, localhost,
                                             logDebug, logInfo,
                                             testBankSecretKey)
import qualified RSCoin.Mintette            as M
import qualified RSCoin.Notary              as N
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
                                             MintetteNumber, NotaryInfo (..),
                                             Scenario (..), TestContext (..),
                                             TestEnv, UserInfo (..), UserNumber,
                                             buser, isActive, port, publicKey,
                                             secretKey, state, users)
import qualified Test.RSCoin.Full.Mintette  as TM

periodDelta :: Second
periodDelta = defaultPeriodDelta

-- | Start all servers/workers and create TestContext.
-- FIXME: we probably need to closeState here
mkTestContext
    :: WorkMode m
    => MintetteNumber -> UserNumber -> Scenario -> m TestContext
mkTestContext mNum uNum scen = do
    binfo <- BankInfo <$> bankKeyPair <*> liftIO B.openMemState
    ninfo <- NotaryInfo <$> liftIO N.openMemState
    minfos <- mapM mkMintette [0 .. mNum - 1]
    buinfo <- UserInfo <$> liftIO U.openMemState
    uinfos <-
        replicateM (fromIntegral uNum) $ UserInfo <$> liftIO U.openMemState
    isActiveVar <- liftIO newEmptyMVar
    logInfo "Initializing system…"
    runMintettes isActiveVar minfos scen
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    mapM_ (addMintetteToBank binfo) minfos
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    runNotary isActiveVar ninfo
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    runBank isActiveVar binfo
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    initBUser buinfo bankSk
    mapM_ initUser uinfos
    let ctx = TestContext binfo minfos ninfo buinfo uinfos scen isActiveVar
    sendInitialCoins ctx
    wait $ for periodDelta sec
    logInfo "Successfully initialized system"
    return ctx
  where
    mkMintette idx =
        MintetteInfo <$> liftIO keyGen <*> liftIO M.openMemState <*>
        pure (2300 + fromIntegral idx)
    bankSk = testBankSecretKey
    bankPk = derivePublicKey bankSk
    bankKeyPair = pure (bankSk, bankPk)
    shortWait = wait $ for 10 ms

-- | Finish everything that's going on in TestEnv.
finishTest :: WorkMode m => TestEnv m ()
finishTest = () <$ (liftIO . flip tryPutMVar () =<< view isActive) --FIXME: close state?

runBank
    :: WorkMode m
    => MVar () -> BankInfo -> m ()
runBank v b = do
    myTId <- myThreadId
    mainIsBusy <- liftIO $ newIORef False
    -- TODO: this code is a modified version of launchBank. Invent
    -- smth to share code
    workWhileMVarEmpty v $
        B.runWorkerWithPeriod
            periodDelta
            mainIsBusy
            (b ^. secretKey)
            (b ^. state)
            Nothing
    workWhileMVarEmpty v $
        B.runExplorerWorker periodDelta mainIsBusy (b ^. secretKey) (b ^. state)
    workWhileMVarEmpty v $ B.serve (b ^. state) myTId pure  -- FIXME: close state `finally`

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

runNotary
    :: WorkMode m
    => MVar () -> NotaryInfo -> m ()
runNotary v n = workWhileMVarEmpty v $ N.serveNotary (n ^. state)

addMintetteToBank
    :: (MonadIO m, WithNamedLogger m)
    => BankInfo -> MintetteInfo -> m ()
addMintetteToBank b mintette = do
    let addedMint = Mintette localhost (mintette ^. port)
        mintPKey  = mintette ^. publicKey
        bankSt    = b ^. state
    logDebug $ sformat ("Adding mintette " % build) addedMint
    update' bankSt $ B.AddMintette addedMint mintPKey
    logDebug $ sformat ("Added mintette " % build) addedMint

initBUser
    :: WorkMode m
    => UserInfo
    -> SecretKey
    -> m ()
initBUser bu bankSk =
    U.initStateBank (bu ^. state) (bankUserAddressesCount - 1) bankSk

initUser
    :: WorkMode m
    => UserInfo -> m ()
initUser user = U.initState (user ^. state) userAddressesCount Nothing

sendInitialCoins
    :: WorkMode m
    => TestContext -> m ()
sendInitialCoins ctx = do
    genesisIdx <-
        fromMaybe reportFatalError <$>
        U.genesisAddressIndex (ctx ^. buser . state)
    runReaderT (mapM_ doAction $ actions genesisIdx) ctx
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
        Just . Coloring . M.fromList . map ((, recip (genericLength allColors)) . getC) $
        nonZeroColors
    actions genesisIdx =
        map
            (\o ->
                  SubmitTransaction
                      Nothing
                      (NonEmpty $ [(genesisIdx, partsToSend)])
                      o
                      coloring)
            outputs
    reportFatalError =
        error
            "[FATAL] RSCoin is broken: genesisAddressIndex returned Nothing for bank user"
