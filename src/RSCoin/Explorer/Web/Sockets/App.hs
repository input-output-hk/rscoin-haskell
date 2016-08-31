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
import           Control.Concurrent.MVar           (MVar, modifyMVar, newMVar,
                                                    readMVar)
import           Control.Lens                      (at, makeLenses, use, view,
                                                    (%=), (+=), (.=), (^.))
import           Control.Monad                     (forever, unless)
import           Control.Monad.Catch               (Handler (Handler),
                                                    SomeException, catches,
                                                    finally)
import           Control.Monad.Extra               (ifM, whenJust)
import           Control.Monad.Reader              (ReaderT, runReaderT)
import           Control.Monad.State               (MonadState, State, runState)
import           Control.Monad.Trans               (MonadIO (liftIO))
import           Data.Acid                         (EventResult, EventState,
                                                    QueryEvent)
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Set                          as S
import           Data.Text                         (Text)
import           Data.Time.Units                   (Second)
import           Formatting                        (build, int, sformat, shown,
                                                    stext, (%))
import qualified Network.WebSockets                as WS

import           Serokell.Util.Concurrent          (threadDelay)
import           Serokell.Util.Text                (listBuilderJSON)

import qualified RSCoin.Core                       as C
import qualified RSCoin.Explorer.AcidState         as DB
import           RSCoin.Explorer.Channel           (Channel, ChannelItem (..),
                                                    readChannel)
import qualified RSCoin.Explorer.Storage           as ES
import           RSCoin.Explorer.Web.Sockets.Types (AddressInfoMsg (..),
                                                    ControlMsg (..),
                                                    ErrorableMsg,
                                                    IncomingMsg (..),
                                                    OutcomingMsg (..),
                                                    ServerError (..))

type SessionId = Word

data ConnectionsState = ConnectionsState
    { _csCounter      :: !Word
    , _csIdToConn     :: !(M.Map SessionId WS.Connection)
    , _csAddrToSessId :: !(M.Map C.Address (S.Set SessionId))
    }

$(makeLenses ''ConnectionsState)

addSession
    :: MonadState ConnectionsState m
    => C.Address -> WS.Connection -> m SessionId
addSession addr conn = do
    i <- use csCounter
    csCounter += 1
    csIdToConn . at i .= Just conn
    csAddrToSessId . at addr %= Just . (maybe (S.singleton i) (S.insert i))
    return i

removeSession
    :: MonadState ConnectionsState m
    => C.Address -> SessionId -> m ()
removeSession addr connId = do
    csIdToConn . at connId .= Nothing
    csAddrToSessId . at addr %= fmap (S.delete connId)

type ConnectionsVar = MVar ConnectionsState

data ServerState = ServerState
    { _ssDataBase    :: !DB.State
    , _ssConnections :: !ConnectionsVar
    }

mkConnectionsState :: ConnectionsState
mkConnectionsState =
    ConnectionsState
    { _csCounter = 0
    , _csIdToConn = M.empty
    , _csAddrToSessId = M.empty
    }

modifyConnectionsState
    :: MonadIO m
    => ConnectionsVar -> State ConnectionsState a -> m a
modifyConnectionsState var st =
    liftIO $ modifyMVar var (pure . swap . runState st)
  where
    swap (a, b) = (b, a)

$(makeLenses ''ServerState)

type ServerMonad = ReaderT ServerState IO

instance C.WithNamedLogger ServerMonad where
    getLoggerName = pure wsLoggerName

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

onSetAddress :: WS.Connection -> C.Address -> ServerMonad ()
onSetAddress conn addr = do
    addrKnown <- query (DB.IsAddressKnown addr)
    sessId <-
        if addrKnown
            then Just <$> establishSession
            else Nothing <$ reportUnknown
    mainLoop conn ((addr, ) <$> sessId)
  where
    establishSession = do
        connections <- view ssConnections
        sessId <- modifyConnectionsState connections $ addSession addr conn
        C.logInfo $
            sformat
                ("Session about " % build % " is established, session id is " %
                 int)
                addr
                sessId
        return sessId
    unknownAddrMsg = sformat ("Address not found: " % build) addr
    reportUnknown = send conn $ OMError $ NotFound $ unknownAddrMsg

onGetTransaction :: WS.Connection -> C.TransactionId -> ServerMonad ()
onGetTransaction conn tId = do
    C.logDebug $ sformat ("Transaction " % build % " is requested") tId
    let errMsg = sformat ("Transaction " % build % " not found") tId
    send conn . maybe (OMError $ NotFound errMsg) OMTransaction =<<
        query (DB.GetTxExtended tId)
    mainLoop conn Nothing

receiveControlMessage
    :: (C.Address -> ServerMonad ())
    -> (C.TransactionId -> ServerMonad ())
    -> (Text -> ServerMonad ())
    -> (ControlMsg -> ServerMonad ())
receiveControlMessage setAddressCB getTransactionCB errorCB msg =
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
                        (pure False)
                        (True <$ getTransactionCB txId)
                        (query (DB.IsTransactionKnown txId))
            results <- sequence [tryAddress, tryTransaction]
            unless (or results) $ errorCB errMsg

sendNotFound :: WS.Connection -> Text -> ServerMonad ()
sendNotFound conn = send conn . OMError . NotFound

onAddressInfo :: WS.Connection -> AddressInfoMsg -> C.Address -> ServerMonad ()
onAddressInfo conn msg addr =
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
                    ("Transactions [" % int % ", " % int % "] pointing to " % build %
                     " are requested")
                    lo
                    hi
                    addr
            send conn . uncurry (OMTransactions addr) =<<
                query (DB.GetAddressTransactions addr indices)

mainLoop :: WS.Connection -> Maybe (C.Address, SessionId) -> ServerMonad ()
mainLoop conn addrAndSess = forever (recv conn "InputMsg" onReceive) `finally` dropSession
  where
    onReceive (IMControl cm) = do
        dropSession
        receiveControlMessage
            (onSetAddress conn)
            (onGetTransaction conn)
            (\e -> sendNotFound conn e >> mainLoop conn Nothing)
            cm
    onReceive (IMAddrInfo am) = do
        maybe onUnexpectedAddrInfo (onAddressInfo conn am . fst) addrAndSess
        mainLoop conn addrAndSess
    onUnexpectedAddrInfo = do
        send conn . OMError . LogicError $
            "Received AddressInfo message while context doesn't store Address"
    dropSession =
        whenJust addrAndSess $
        \(addr, sessId) -> do
            connections <- view ssConnections
            modifyConnectionsState connections $ removeSession addr sessId

handler :: WS.PendingConnection -> ServerMonad ()
handler pendingConn = do
    C.logDebug "There is a new pending connection"
    conn <- liftIO $ WS.acceptRequest pendingConn
    C.logDebug "Accepted new connection"
    liftIO $ WS.forkPingThread conn 30
    recv conn "Control Message" $
        receiveControlMessage
            (onSetAddress conn)
            (onGetTransaction conn)
            (sendNotFound conn)

sender :: Channel -> ServerMonad ()
sender channel =
    foreverSafe $
    do ChannelItem {ciTransactions = txs} <- readChannel channel
       C.logDebug "There is a new ChannelItem in Channel"
       let inputs = concatMap C.txInputs txs
           outputs = concatMap C.txOutputs txs
           outputAddresses = S.fromList $ map fst outputs
           inputToAddr :: C.AddrId -> ServerMonad (Maybe C.Address)
           inputToAddr (txId, idx, _) =
               fmap (fst . (!! idx) . C.txOutputs) <$>
               query (DB.GetTx txId)
       affectedAddresses <-
           mappend outputAddresses . S.fromList . catMaybes <$>
           mapM inputToAddr inputs
       C.logDebug $
           sformat
               ("Affected addresses are: " % build)
               (listBuilderJSON affectedAddresses)
       mapM_ notifyAboutAddressUpdate affectedAddresses
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

notifyAboutAddressUpdate :: C.Address -> ServerMonad ()
notifyAboutAddressUpdate addr = do
    connectionsState <- liftIO . readMVar =<< view ssConnections
    msgBalance <- uncurry (OMBalance addr) <$> query (DB.GetAddressBalance addr)
    msgTxNumber <-
        uncurry (OMTxNumber addr) <$> query (DB.GetAddressTxNumber addr)
    let sessIds = fromMaybe S.empty $ connectionsState ^. csAddrToSessId . at addr
        idToConn i = connectionsState ^. csIdToConn . at i
        foldrStep sessId l = maybe l (: l) $ idToConn sessId
        connections = S.foldr' foldrStep [] sessIds
        sendToAll msg = mapM_ (flip send msg) connections
    mapM_ sendToAll [msgBalance, msgTxNumber]

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
           app pc = runReaderT (handler pc) ss
       app <$ forkIO (runReaderT (sender channel) ss)
