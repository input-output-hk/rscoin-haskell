{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

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
import           Control.Monad                     (forever, when)
import           Control.Monad.Catch               (Handler (Handler),
                                                    SomeException, catches,
                                                    finally)
import           Control.Monad.Catch               (MonadThrow (throwM), catch)
import           Control.Monad.Extra               (notM, whenM)
import           Control.Monad.Reader              (ReaderT, runReaderT)
import           Control.Monad.State               (MonadState, State, runState)
import           Control.Monad.Trans               (MonadIO (liftIO))
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Set                          as S
import           Data.Text                         (pack)
import           Data.Time.Units                   (Second)
import           Formatting                        (build, int, sformat, shown,
                                                    (%))
import qualified Network.WebSockets                as WS

import           Serokell.Util.Concurrent          (threadDelay)
import           Serokell.Util.Text                (listBuilderJSON)

import qualified RSCoin.Core                       as C

import qualified RSCoin.Explorer.AcidState         as DB
import           RSCoin.Explorer.Channel           (Channel, ChannelItem (..),
                                                    readChannel)
import           RSCoin.Explorer.Error             (ExplorerError (EENotFound))
import           RSCoin.Explorer.Summaries         (TransactionSummary (txsOutputs))
import           RSCoin.Explorer.Web.Sockets.Types (AddressInfoMsg (..),
                                                    ErrorableMsg,
                                                    IntroductoryMsg (..),
                                                    OutcomingMsg (..),
                                                    ServerError (NotFound))

type ConnectionId = Word

data ConnectionsState = ConnectionsState
    { _csCounter      :: !Word
    , _csIdToConn     :: !(M.Map ConnectionId WS.Connection)
    , _csAddrToConnId :: !(M.Map C.Address (S.Set ConnectionId))
    }

$(makeLenses ''ConnectionsState)

addConnection
    :: MonadState ConnectionsState m
    => C.Address -> WS.Connection -> m ConnectionId
addConnection addr conn = do
    i <- use csCounter
    csCounter += 1
    csIdToConn . at i .= Just conn
    csAddrToConnId . at addr %= Just . (maybe (S.singleton i) (S.insert i))
    return i

dropConnection
    :: MonadState ConnectionsState m
    => C.Address -> ConnectionId -> m ()
dropConnection addr connId = do
    csIdToConn . at connId .= Nothing
    csAddrToConnId . at addr %= fmap (S.delete connId)

type ConnectionsVar = MVar ConnectionsState

data ServerState = ServerState
    { _ssDataBase    :: DB.State
    , _ssConnections :: ConnectionsVar
    }

mkConnectionsState :: ConnectionsState
mkConnectionsState =
    ConnectionsState
    { _csCounter = 0
    , _csIdToConn = M.empty
    , _csAddrToConnId = M.empty
    }

modifyConnectionsState
    :: MonadIO m
    => ConnectionsVar -> State ConnectionsState a -> m a
modifyConnectionsState var st =
    liftIO $ modifyMVar var (pure . swap . runState st)
  where
    swap (a,b) = (b, a)

$(makeLenses ''ServerState)

type ServerMonad = ReaderT ServerState IO

instance C.WithNamedLogger ServerMonad where
    getLoggerName = liftIO $ C.getLoggerName

send
    :: MonadIO m
    => WS.Connection -> OutcomingMsg -> m ()
send conn = liftIO . WS.sendTextData conn

recv
    :: (MonadIO m, WS.WebSocketsData (ErrorableMsg msg))
    => WS.Connection -> (msg -> m ()) -> m ()
recv conn callback =
    either (send conn . OMError) callback =<<
    liftIO (WS.receiveData conn)

wsLoggerName :: C.LoggerName
wsLoggerName = C.LoggerName "explorer WS"

introduceAddress :: Bool -> WS.Connection -> C.Address -> ServerMonad ()
introduceAddress sendResponseOnError conn addr = do
    checkAddressExistence
    connections <- view ssConnections
    connId <- modifyConnectionsState connections $ addConnection addr conn
    C.logInfo $
        sformat
            ("Session about " % build % " is established, connection id is " %
             int)
            addr
            connId
    addressInfoHandler addr conn `finally`
        modifyConnectionsState connections (dropConnection addr connId)
  where
    checkAddressExistence = do
        whenM (notM $ flip DB.query (DB.AddressExists addr) =<< view ssDataBase) $
            do let e = "Address not found"
               when sendResponseOnError $ send conn $ OMError $ NotFound e
               throwM $ EENotFound e
        send conn $ OMSessionEstablished addr

introduceTransaction :: WS.Connection -> C.TransactionId -> ServerMonad ()
introduceTransaction conn tId = do
    C.logDebug $ sformat ("Transaction " % build % " is requested") tId
    send conn .
        maybe (OMError $ NotFound "Transaction not found") OMTransaction =<<
        flip DB.query (DB.GetTxSummary tId) =<< view ssDataBase

changeInfo :: WS.Connection -> C.Address -> C.TransactionId -> ServerMonad ()
changeInfo conn addr tId =
    introduceAddress False conn addr `catch` tryTransaction
    where
    tryTransaction :: ExplorerError -> ServerMonad ()
    tryTransaction (EENotFound _) = introduceTransaction conn tId
    tryTransaction e              = throwM e

handler :: WS.PendingConnection -> ServerMonad ()
handler pendingConn = do
    C.logDebug "There is a new pending connection"
    conn <- liftIO $ WS.acceptRequest pendingConn
    C.logDebug "Accepted new connection"
    liftIO $ WS.forkPingThread conn 30
    forever $ recv conn $ onReceive conn
  where
    onReceive conn (IMAddressInfo addr')   = introduceAddress True conn addr'
    onReceive conn (IMTransactionInfo tId) = introduceTransaction conn tId
    onReceive conn (IMInfo addr' tId)      = changeInfo conn addr' tId

addressInfoHandler :: C.Address -> WS.Connection -> ServerMonad ()
addressInfoHandler addr conn = forever $ recv conn onReceive
  where
    onReceive AIGetBalance = do
        C.logDebug $ sformat ("Balance of " % build % " is requested") addr
        send conn . uncurry OMBalance =<<
            flip DB.query (DB.GetAddressBalance addr) =<< view ssDataBase
    onReceive AIGetTxNumber = do
        C.logDebug $
            sformat
                ("Number of transactions pointing to " % build %
                 " is requested")
                addr
        send conn . uncurry OMTxNumber . fmap (pack . show) =<<
            flip DB.query (DB.GetAddressTxNumber addr) =<< view ssDataBase
    onReceive (AIGetTransactions indices@(lo,hi)) = do
        C.logDebug $
            sformat
                ("Transactions [" % int % ", " % int % "] pointing to " % build %
                 " are requested")
                lo
                hi
                addr
        send conn . uncurry OMTransactions =<<
            flip DB.query (DB.GetAddressTransactions addr indices) =<<
            view ssDataBase
    onReceive (AIChangeAddress addr') = introduceAddress True conn addr'
    onReceive (AIChangeInfo addr' tId) = changeInfo conn addr' tId

sender :: Channel -> ServerMonad ()
sender channel =
    foreverSafe $
    do ChannelItem{ciTransactions = txs} <- readChannel channel
       C.logDebug "There is a new ChannelItem in Channel"
       st <- view ssDataBase
       let inputs = concatMap C.txInputs txs
           outputs = concatMap C.txOutputs txs
           outputAddresses = S.fromList $ map fst outputs
           inputToAddr
               :: MonadIO m
               => C.AddrId -> m (Maybe C.Address)
           inputToAddr (txId,idx,_) =
               fmap (fst . (!! idx) . txsOutputs) <$>
               DB.query st (DB.GetTxSummary txId)
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
    st <- view ssDataBase
    connectionsState <- liftIO . readMVar =<< view ssConnections
    msgBalance <- uncurry OMBalance <$> DB.query st (DB.GetAddressBalance addr)
    msgTxNumber <-
        uncurry OMTxNumber . fmap (pack . show) <$> DB.query st (DB.GetAddressTxNumber addr)
    let connIds =
            fromMaybe S.empty $ connectionsState ^. csAddrToConnId . at addr
        idToConn i = connectionsState ^. csIdToConn . at i
        foldrStep connId l = maybe l (: l) $ idToConn connId
        connections = S.foldr' foldrStep [] connIds
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
