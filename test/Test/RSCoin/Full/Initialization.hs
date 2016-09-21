{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}

-- | This module defines how to initialize RSCoin.

module Test.RSCoin.Full.Initialization
       ( mkTestContext, finishTest
       , bankUserAddressesCount, userAddressesCount
       ) where

import           Control.Exception          (Exception, assert,
                                             AsyncException (ThreadKilled))
import           Control.Lens               (view, (^.))
import           Control.Monad              (forM_, replicateM)
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.Trans        (MonadIO (liftIO), lift)
import qualified Data.IntMap.Strict         as M
import           Data.IORef                 (newIORef)
import           Data.List                  (genericLength)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Optional              (Optional (Default))
import           Formatting                 (build, sformat, (%))
import           Test.QuickCheck            (NonEmptyList (..))

import           Control.TimeWarp.Logging   (LoggerName (..), modifyLoggerName,
                                             setLoggerName)
import           Control.TimeWarp.Timed     (Second, for, ms,
                                             wait)
import           Test.RSCoin.Full.Threads   (makeThreadsController)
import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Color (..), Mintette (..),
                                             SecretKey, WithNamedLogger,
                                             WorkMode, bankLoggerName,
                                             defaultPeriodDelta,
                                             derivePublicKey, keyGen, localhost,
                                             logDebug, logInfo,
                                             mintetteLoggerName,
                                             notaryLoggerName,
                                             testBankSecretKey,
                                             testNotarySecretKey)
import qualified RSCoin.Mintette            as M
import qualified RSCoin.Notary              as N
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
                                             buser, stopNodes, port, publicKey,
                                             secretKey, state, users)
import qualified Test.RSCoin.Full.Mintette  as TM

periodDelta :: Second
periodDelta = defaultPeriodDelta

-- | Start all servers/workers and create TestContext.
mkTestContext
    :: WorkMode m
    => MintetteNumber -> UserNumber -> Scenario -> m (TestContext m)
mkTestContext mNum uNum scen = do
    binfo <- BankInfo <$> bankKeyPair <*> liftIO B.openMemState
    ninfo <- NotaryInfo <$> liftIO (N.openMemState [] Default Default)
    minfos <- mapM mkMintette [0 .. mNum - 1]
    buinfo <- UserInfo <$> liftIO U.openMemState
    uinfos <-
        replicateM (fromIntegral uNum) $ UserInfo <$> liftIO U.openMemState
    (forkTmp, killAll) <- makeThreadsController ThreadKilled
    logInfo "Initializing system…"
    runMintettes forkTmp minfos scen
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    mapM_ (addMintetteToBank binfo) minfos
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    runNotary forkTmp ninfo
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    runBank forkTmp binfo
    shortWait -- DON'T TOUCH IT (you can, but take responsibility then)
    initBUser buinfo bankSk
    mapM_ initUser uinfos
    let ctx = TestContext binfo minfos ninfo buinfo uinfos scen killAll
    sendInitialCoins ctx
    wait $ for periodDelta
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

data TestFinished = TestFinished
    deriving (Show)

instance Exception TestFinished

-- | Finish everything that's going on in TestEnv.
finishTest :: WorkMode m => TestEnv m m ()
finishTest = view stopNodes >>= lift

runBank
    :: WorkMode m
    => (m () -> m ()) -> BankInfo -> m ()
runBank forkTmp b =
    setLoggerName bankLoggerName $
      do
        mainIsBusy <- liftIO $ newIORef False
        -- TODO: this code is a modified version of launchBank. Invent
        -- smth to share code
        forkTmp $
            modifyLoggerName (<> "server") $
                B.serve (b ^. state) (b ^. secretKey) mainIsBusy
        wait $ for 10 ms
        forkTmp $
            modifyLoggerName (<> "worker") $
                B.runWorker
                    periodDelta
                    (b ^. secretKey)
                    (b ^. state)
        forkTmp $
            modifyLoggerName (<> "explorer-worker") $
                B.runExplorerWorker
                    mainIsBusy
                    (b ^. secretKey)
                    (b ^. state)

runMintettes
    :: WorkMode m
    => (m () -> m ()) -> [MintetteInfo] -> Scenario -> m ()
runMintettes forkTmp mts scen =
    setLoggerName mintetteLoggerName $
        case scen of
            DefaultScenario ->
                withEnumedLogger_ (TM.defaultMintetteInit forkTmp) mts
            (MalfunctioningMintettes d) -> do
                let (other,normal) = splitAt (partSize d) mts
                withEnumedLogger_ (TM.defaultMintetteInit forkTmp) normal
                modifyLoggerName (<> "mulfunctioned") $
                    withEnumedLogger_ (TM.malfunctioningMintetteInit forkTmp)
                        other
            _ -> error "Test.Action.runMintettes not implemented"
  where
    partSize :: Double -> Int
    partSize d = assert (d >= 0 && d <= 1) $ floor $ genericLength mts * d

    withEnumedLogger_ :: WorkMode m => (a -> m b) -> [a] -> m ()
    withEnumedLogger_ f l = forM_ (zip [1::Int ..] l) $
        \(no, e) -> modifyLoggerName (<> LoggerName (show no)) $ f e

runNotary
    :: WorkMode m
    => (m () -> m ()) -> NotaryInfo -> m ()
runNotary forkTmp n =
    forkTmp $
    setLoggerName notaryLoggerName $
    N.serveNotary testNotarySecretKey (n ^. state)

addMintetteToBank
    :: (MonadIO m, WithNamedLogger m)
    => BankInfo -> MintetteInfo -> m ()
addMintetteToBank b mintette = do
    let addedMint = Mintette localhost (mintette ^. port)
        mintPKey  = mintette ^. publicKey
        bankSt    = b ^. state
    logDebug $ sformat ("Adding mintette " % build) addedMint
    B.update bankSt $ B.AddMintette addedMint mintPKey
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
    => TestContext m -> m ()
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
        Just . Coloring . M.fromList . map ((, recip (genericLength allColors)) . getColor) $
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
