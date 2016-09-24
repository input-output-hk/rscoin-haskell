{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Logic of Explorer WebSockets Server.
module RSCoin.Explorer.Web.Sockets.App
       ( wsLoggerName
       , mkWsApp
       ) where

import           Control.Concurrent                (forkIO)
import           Control.Concurrent.MVar           (MVar, modifyMVar, newMVar, readMVar)
import           Control.Lens                      (at, makeLenses, preuse, use, view,
                                                    (%=), (+=), (.=), (^.), _Just)
import           Control.Monad                     (forever, join, unless)
import           Control.Monad.Catch               (Handler (Handler), MonadCatch,
                                                    MonadMask, MonadThrow, SomeException,
                                                    catches, finally, throwM)
import           Control.Monad.Extra               (ifM, whenJust, whenJustM, whenM)
import           Control.Monad.Reader              (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State               (MonadState, State, runState)
import           Control.Monad.Trans               (MonadIO (liftIO))
import           Data.Acid                         (EventResult, EventState, QueryEvent)
import           Data.Bifunctor                    (second)
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Set                          as S
import           Data.Text                         (Text)
import           Data.Time.Units                   (Second)
import           Formatting                        (build, int, sformat, shown, stext,
                                                    (%))
import qualified Network.WebSockets                as WS

import           Serokell.Util.Common              (indexedSubList)
import           Serokell.Util.Concurrent          (threadDelay)
import           Serokell.Util.Text                (listBuilderJSON)

import qualified RSCoin.Core                       as C
import qualified RSCoin.Explorer.AcidState         as DB
import           RSCoin.Explorer.Channel           (Channel, ChannelItem (..),
                                                    readChannel)
import           RSCoin.Explorer.Error             (ExplorerError (..))
import qualified RSCoin.Explorer.Storage           as ES
import           RSCoin.Explorer.Web.Sockets.Types (AddressInfoMsg (..), ControlMsg (..),
                                                    ErrorableMsg, HBlockInfoMsg (..),
                                                    IncomingMsg (..), OutcomingMsg (..),
                                                    ServerError (..))

type SessionId = Word

data ClientContext = ClientContext
    { _ccAddress    :: !(Maybe C.Address)
    , _ccHBlock     :: !(Maybe C.PeriodId)
    , _ccConnection :: !WS.Connection
    }

mkClientContext :: WS.Connection -> ClientContext
mkClientContext = ClientContext Nothing Nothing

$(makeLenses ''ClientContext)

data ConnectionsState = ConnectionsState
    { -- | Conuter used to generate SessionIds.
      _csCounter            :: !Word
    , _csClients            :: !(M.Map SessionId ClientContext)
      -- | Sessions subscribed to given address.
    , _csAddressSubscribers :: !(M.Map C.Address (S.Set SessionId))
      -- | Sessions subscribed to notifications about new HBLocks.
    , _csHBlocksSubscribers :: !(S.Set SessionId)
    }

$(makeLenses ''ConnectionsState)

startSession
    :: MonadState ConnectionsState m
    => WS.Connection -> m SessionId
startSession conn = do
    i <- use csCounter
    csCounter += 1
    let cc = mkClientContext conn
    i <$ (csClients . at i .= Just cc)

finishSession :: MonadState ConnectionsState m => SessionId -> m ()
finishSession i = whenJustM (use $ csClients . at i) finishSessionDo
  where
    finishSessionDo _ = do
        csClients . at i .= Nothing
        unsubscribeHBlocks i
        unsubscribeAddr i

setClientAddress
    :: MonadState ConnectionsState m
    => SessionId -> Maybe C.Address -> m ()
setClientAddress sessId addr = do
    unsubscribeAddr sessId
    csClients . at sessId . _Just . ccAddress .= addr
    whenJust addr $ subscribeAddr sessId

setClientHBlock
    :: MonadState ConnectionsState m
    => SessionId -> Maybe C.PeriodId -> m ()
setClientHBlock sessId pId = do
    csClients . at sessId . _Just . ccHBlock .= pId
    subscribeHBlocks sessId

subscribeAddr
    :: MonadState ConnectionsState m
    => SessionId -> C.Address -> m ()
subscribeAddr i addr =
    csAddressSubscribers . at addr %= Just .
    (maybe (S.singleton i) (S.insert i))

unsubscribeAddr
    :: MonadState ConnectionsState m
    => SessionId -> m ()
unsubscribeAddr i = do
    addr <- preuse $ csClients . at i . _Just . ccAddress
    whenJust (join addr) unsubscribeDo
  where
    unsubscribeDo a = csAddressSubscribers . at a %= fmap (S.delete i)

subscribeHBlocks
    :: MonadState ConnectionsState m
    => SessionId -> m ()
subscribeHBlocks i = csHBlocksSubscribers %= S.insert i

unsubscribeHBlocks
    :: MonadState ConnectionsState m
    => SessionId -> m ()
unsubscribeHBlocks i = csHBlocksSubscribers %= S.delete i

unsubscribeFully
    :: MonadState ConnectionsState m
    => SessionId -> m ()
unsubscribeFully i = unsubscribeHBlocks i >> unsubscribeAddr i

type ConnectionsVar = MVar ConnectionsState

data ServerState = ServerState
    { _ssDataBase    :: !DB.State
    , _ssConnections :: !ConnectionsVar
    }

mkConnectionsState :: ConnectionsState
mkConnectionsState =
    ConnectionsState
    { _csCounter = 0
    , _csClients = mempty
    , _csAddressSubscribers = mempty
    , _csHBlocksSubscribers = mempty
    }

modifyConnectionsStateDo
    :: MonadIO m
    => ConnectionsVar -> State ConnectionsState a -> m a
modifyConnectionsStateDo var st =
    liftIO $ modifyMVar var (pure . swap . runState st)
  where
    swap (a, b) = (b, a)

$(makeLenses ''ServerState)

newtype ServerMonad a = ServerMonad
    { getServerMonad :: ReaderT ServerState IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader ServerState)

instance C.WithNamedLogger ServerMonad where
    getLoggerName = pure wsLoggerName

    modifyLoggerName = const id

modifyConnectionsState :: State ConnectionsState a -> ServerMonad a
modifyConnectionsState st =
    flip modifyConnectionsStateDo st =<< view ssConnections

send
    :: MonadIO m
    => WS.Connection -> OutcomingMsg -> m ()
send conn = liftIO . WS.sendTextData conn

recv
    :: (MonadIO m, WS.WebSocketsData (ErrorableMsg msg))
    => WS.Connection -> Text -> (msg -> m ()) -> m ()
recv conn typeName callback =
    either (send conn . OMError . ParseError typeName) callback =<<
    liftIO (WS.receiveData conn)

wsLoggerName :: C.LoggerName
wsLoggerName = C.LoggerName "explorer WS"

query
    :: (EventState event ~ ES.Storage, QueryEvent event)
    => event -> ServerMonad (EventResult event)
query event = flip DB.query event =<< view ssDataBase

myContext :: SessionId -> ServerMonad ClientContext
myContext sessId = do
    ctx <-
        view (csClients . at sessId) <$>
        (liftIO . readMVar =<< view ssConnections)
    maybe
        (throwM $ EEInternalError $ sformat ("session id not found " % int) sessId)
        pure
        ctx

myConnection :: SessionId -> ServerMonad WS.Connection
myConnection sessId = (^. ccConnection) <$> myContext sessId

myAddress :: SessionId -> ServerMonad (Maybe C.Address)
myAddress sessId = (^. ccAddress) <$> myContext sessId

myBlockId :: SessionId -> ServerMonad (Maybe C.PeriodId)
myBlockId sessId = (^. ccHBlock) <$> myContext sessId

checkLimit
    :: (Integral n, Ord n)
    => SessionId -> Word -> (n, n) -> Text -> ServerMonad Bool
checkLimit sessId limit (lo, hi) unitsName
    | lo >= hi = pure True
    | fromIntegral (hi - lo) <= limit = pure True
    | otherwise = do
        let msg =
                sformat
                    ("please request no more than " % int % " " % stext %
                     " (you requested " %
                     int %
                     ")")
                    limit
                    unitsName
                    (hi - lo)
        conn <- myConnection sessId
        False <$ send conn (OMError $ LimitExceededError msg)

onSetAddress :: SessionId -> C.Address -> ServerMonad ()
onSetAddress sessId addr = do
    addrKnown <- query (DB.IsAddressKnown addr)
    if addrKnown
        then setAndLog
        else reportUnknown
    recvLoop sessId
  where
    setAndLog = do
        modifyConnectionsState $ setClientAddress sessId (Just addr)
        C.logInfo $
            sformat
                ("Session about " % build % " is established, session id is " %
                 int)
                addr
                sessId
        notifyAboutAddressUpdate sessId addr
    unknownAddrMsg = sformat ("Address not found: " % build) addr
    reportUnknown = do
        C.logInfo $
            sformat
                ("Unknown address requested " % build % ", sessiod id is " % int)
                addr
                sessId
        conn <- myConnection sessId
        send conn $ OMError $ NotFound $ unknownAddrMsg

onGetTransaction :: SessionId -> C.TransactionId -> ServerMonad ()
onGetTransaction sessId tId = do
    C.logDebug $ sformat ("Transaction " % build % " is requested") tId
    let errMsg = sformat ("Transaction " % build % " not found") tId
    conn <- myConnection sessId
    modifyConnectionsState $ unsubscribeFully sessId
    send conn . maybe (OMError $ NotFound errMsg) (OMTransaction tId) =<<
        query (DB.GetTxExtended tId)
    recvLoop sessId

onSetHBlock :: SessionId -> C.PeriodId -> ServerMonad ()
onSetHBlock sessId pId = do
    good <- ((pId >= 0) &&) . (pId <) <$> query DB.GetExpectedPeriodId
    if good
        then setAndLog
        else reportBad
    recvLoop sessId
  where
    setAndLog = do
        modifyConnectionsState $ setClientHBlock sessId (Just pId)
        C.logInfo $
            sformat
                ("Session about HBlock #" % int %
                 " is established, session id is " %
                 int)
                pId
                sessId
    badIdMsg = sformat ("Invalid period id: " % int) pId
    reportBad = do
        C.logInfo $
            sformat
                ("Invalid HBlock is requested (" % build % "), sessiod id is " %
                 int)
                pId
                sessId
        conn <- myConnection sessId
        send conn $ OMError $ NotFound $ badIdMsg

onGetBlockchainHeight :: SessionId -> ServerMonad ()
onGetBlockchainHeight sessId = do
    C.logDebug "Blockchain height requested"
    conn <- myConnection sessId
    send conn . OMBlockchainHeight . pred =<< query DB.GetExpectedPeriodId
    recvLoop sessId

blocksOverviewLimit :: Num a => a
blocksOverviewLimit = 40

onGetBlocksOverview :: SessionId -> (C.PeriodId, C.PeriodId) -> ServerMonad ()
onGetBlocksOverview sessId range = do
    C.logDebug "Blocks overview requested"
    whenM (checkLimit sessId blocksOverviewLimit range "block metadatas") $
        do conn <- myConnection sessId
           send conn . OMBlocksOverview . map (second C.wmMetadata) =<<
               query (DB.GetHBlocksExtended range)
    recvLoop sessId

transactionsLimit :: Num a => a
transactionsLimit = 80

onGetTransactionsGlobal :: SessionId -> (Word, Word) -> ServerMonad ()
onGetTransactionsGlobal sessId range = do
    C.logDebug "Transactions from global history requested"
    whenM (checkLimit sessId transactionsLimit range "transactions") $
        do conn <- myConnection sessId
           send conn . uncurry OMTransactionsGlobal =<<
               query (DB.GetTxsGlobal range)
    recvLoop sessId

onSubscribeNewBlock :: SessionId -> ServerMonad ()
onSubscribeNewBlock sessId = do
    C.logDebug $ sformat ("Client " % int % " subscribes to new blocks") sessId
    modifyConnectionsState (subscribeHBlocks sessId)
    recvLoop sessId

onSubscribeAddress :: SessionId -> C.Address -> ServerMonad ()
onSubscribeAddress sessId addr = do
    C.logDebug $
        sformat
            ("Client " % int % " subscribes to updates about " % build)
            sessId
            addr
    modifyConnectionsState (subscribeAddr sessId addr)
    recvLoop sessId

receiveControlMessage :: SessionId
                      -> (Text -> ServerMonad ())
                      -> ControlMsg
                      -> ServerMonad ()
receiveControlMessage sessId errorCB msg =
    case msg of
        CMSetAddress addr -> setAddressCB addr
        CMGetTransaction txId -> getTransactionCB txId
        CMSmart text -> do
            let errMsg =
                    sformat
                        ("Can't find anything (Address or Transaction) identified by string: " %
                         stext)
                        text
                tryAddress =
                    maybe (pure False) (tryAddressDo . C.Address) $
                    C.constructPublicKey text
                tryAddressDo addr = do
                    ifM
                        (query (DB.IsAddressKnown addr))
                        (True <$ setAddressCB addr)
                        (pure False)
                tryTransaction =
                    either (const $ pure False) tryTransactionDo $
                    C.parseHash text
                tryTransactionDo txId =
                    ifM
                        (query (DB.IsTransactionKnown txId))
                        (True <$ getTransactionCB txId)
                        (pure False)
            results <- sequence [tryAddress, tryTransaction]
            unless (or results) $ errorCB errMsg
        CMSetHBlock pId -> onSetHBlock sessId pId
        CMGetBlockchainHeight -> onGetBlockchainHeight sessId
        CMGetBlocksOverview range -> onGetBlocksOverview sessId range
        CMGetTransactionsGlobal range -> onGetTransactionsGlobal sessId range
        CMSubscribeNewBlock -> onSubscribeNewBlock sessId
        CMSubscribeAddress addr -> onSubscribeAddress sessId addr
  where
    setAddressCB = onSetAddress sessId
    getTransactionCB = onGetTransaction sessId

sendNotFound :: SessionId -> Text -> ServerMonad ()
sendNotFound sessId t = do
    conn <- myConnection sessId
    send conn . OMError . NotFound $ t

onAddressInfo :: SessionId -> AddressInfoMsg -> C.Address -> ServerMonad ()
onAddressInfo sessId msg addr = do
    conn <- myConnection sessId
    case msg of
        AIGetBalance -> do
            C.logDebug $ sformat ("Balance of " % build % " is requested") addr
            send conn . uncurry (OMBalance addr) =<<
                query (DB.GetAddressBalance addr)
        AIGetTxNumber -> do
            C.logDebug $
                sformat
                    ("Number of transactions pointing to " % build %
                     " is requested")
                    addr
            send conn . uncurry (OMTxNumber addr) =<<
                query (DB.GetAddressTxNumber addr)
        AIGetTransactions indices@(lo, hi) -> do
            C.logDebug $
                sformat
                    ("Transactions [" % int % ", " % int % "] pointing to " %
                     build %
                     " are requested")
                    lo
                    hi
                    addr
            whenM (checkLimit sessId transactionsLimit indices "transactions") $
                send conn . uncurry (OMAddrTransactions addr) =<<
                query (DB.GetAddressTransactions addr indices)

onHBlockInfo :: SessionId -> HBlockInfoMsg -> C.PeriodId -> ServerMonad ()
onHBlockInfo sessId msg blkId = do
    conn <- myConnection sessId
    blkExtendedMaybe <- query (DB.GetHBlockExtended blkId)
    let sendError =
            send conn $
            OMError $ NotFound $ sformat ("HBlock #" % int % " not found") blkId
        proceed C.WithMetadata {wmValue = C.HBlock {..}, wmMetadata = ext} = do
            txExtensions <- query (DB.GetTxExtensions blkId)
            case msg of
                HIGetMetadata -> send conn . OMBlockMetadata blkId $ ext
                (HIGetTransactions indices) ->
                    whenM
                        (checkLimit
                             sessId
                             transactionsLimit
                             indices
                             "transactions") $
                    send conn . OMBlockTransactions blkId $
                    indexedSubList indices $
                    zipWith C.WithMetadata hbTransactions txExtensions
    maybe sendError proceed blkExtendedMaybe

recvLoop :: SessionId -> ServerMonad ()
recvLoop sessId = do
    conn <- myConnection sessId
    let onReceive (IMControl cm) = do
            receiveControlMessage
                sessId
                (\e -> sendNotFound sessId e >> recvLoop sessId)
                cm
        onReceive (IMAddrInfo am) = do
            addr <- myAddress sessId
            maybe onUnexpectedAddrInfo (onAddressInfo sessId am) addr
            recvLoop sessId
        onReceive (IMHBlockInfo msg) = do
            blkId <- myBlockId sessId
            maybe onUnexpectedHBlockInfo (onHBlockInfo sessId msg) blkId
            recvLoop sessId
        onUnexpectedAddrInfo =
            send conn . OMError . LogicError $
            "Received AddressInfo message while context doesn't store Address"
        onUnexpectedHBlockInfo =
            send conn . OMError . LogicError $
            "Received HBlockInfo message while context doesn't store HBlock id"
    forever (recv conn "InputMsg" onReceive) `finally`
        modifyConnectionsState (finishSession sessId)

handler :: WS.PendingConnection -> ServerMonad ()
handler pendingConn = do
    C.logDebug "There is a new pending connection"
    conn <- liftIO $ WS.acceptRequest pendingConn
    recv conn "Control Message" $ onControlMsg conn
  where
    onControlMsg conn (IMControl cm) = do
        C.logDebug "Accepted new connection"
        liftIO $ WS.forkPingThread conn 30
        sessId <- modifyConnectionsState $ startSession conn
        receiveControlMessage sessId (sendNotFound sessId) cm
    onControlMsg conn msg = do
        let e = sformat ("Expected IMControl msg but got " % shown % " .") msg
        C.logDebug e
        send conn . OMError $ LogicError e

sender :: Channel -> ServerMonad ()
sender channel = foreverSafe $ onNewChannelItem =<< readChannel channel
  where
    foreverSafe :: ServerMonad () -> ServerMonad ()
    foreverSafe action = do
        let catchConnectionError :: WS.ConnectionException -> ServerMonad ()
            catchConnectionError e =
                C.logError $ sformat ("Connection error happened: " % shown) e
            catchWhateverError :: SomeException -> ServerMonad ()
            catchWhateverError e = do
                C.logError $ sformat ("Strange error happened: " % shown) e
                threadDelay (2 :: Second)
        action `catches`
            [Handler catchConnectionError, Handler catchWhateverError]
        foreverSafe action

onNewChannelItem :: ChannelItem -> ServerMonad ()
onNewChannelItem ChannelItem {ciTransactions = txs} = do
    C.logDebug "There is a new ChannelItem in Channel"
    let inputs = concatMap C.txInputs txs
        outputs = concatMap C.txOutputs txs
        outputAddresses = S.fromList $ map fst outputs
        inputToAddr :: C.AddrId -> ServerMonad (Maybe C.Address)
        inputToAddr (txId, idx, _) =
            fmap (fst . (!! idx) . C.txOutputs) <$> query (DB.GetTx txId)
    affectedAddresses <-
        mappend outputAddresses . S.fromList . catMaybes <$>
        mapM inputToAddr inputs
    C.logDebug $
        sformat
            ("Affected addresses are: " % build)
            (listBuilderJSON affectedAddresses)
    mapM_ notifySubscribersAboutAddressUpdate affectedAddresses
    notifyAboutNewHBlock

mkNotificationMessages :: C.Address -> ServerMonad [OutcomingMsg]
mkNotificationMessages addr = do
    msgBalance <- uncurry (OMBalance addr) <$> query (DB.GetAddressBalance addr)
    msgTxNumber <-
        uncurry (OMTxNumber addr) <$> query (DB.GetAddressTxNumber addr)
    return [msgBalance, msgTxNumber]

notifyAboutAddressUpdate :: SessionId -> C.Address -> ServerMonad ()
notifyAboutAddressUpdate sessId addr = do
    connection <- myConnection sessId
    mapM_ (send connection) =<< mkNotificationMessages addr

notifySubscribersAboutAddressUpdate :: C.Address -> ServerMonad ()
notifySubscribersAboutAddressUpdate addr = do
    connectionsState <- liftIO . readMVar =<< view ssConnections
    let sessIds =
            S.toList $ fromMaybe S.empty $ connectionsState ^.
            csAddressSubscribers .
            at addr
    connections <- mapM myConnection sessIds
    let sendToAll msg = mapM_ (flip send msg) connections
    mapM_ sendToAll =<< mkNotificationMessages addr

notifyAboutNewHBlock :: ServerMonad ()
notifyAboutNewHBlock = do
    connectionsState <- liftIO . readMVar =<< view ssConnections
    let sessIds = S.toList $ connectionsState ^. csHBlocksSubscribers
    connections <- mapM myConnection sessIds
    msg <- OMNewBlock . pred <$> query DB.GetExpectedPeriodId
    mapM_ (flip send msg) connections

-- | Given access to Explorer's data base and channel, returns
-- WebSockets server application.
mkWsApp
    :: MonadIO m
    => Channel -> DB.State -> m WS.ServerApp
mkWsApp channel st =
    liftIO $
    do connections <- newMVar mkConnectionsState
       let ss =
               ServerState
               { _ssDataBase = st
               , _ssConnections = connections
               }
           runServerMonad = flip runReaderT ss . getServerMonad
           app pc = runServerMonad (handler pc)
       app <$ forkIO (runServerMonad (sender channel))
