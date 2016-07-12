{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | WebSockets part of Explorer Web Server.

module RSCoin.Explorer.Web.Sockets
       ( mkWsApp
       ) where

import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar   (MVar, modifyMVar, newMVar, readMVar)
import           Control.Lens              (at, makeLenses, use, view, (%=),
                                            (+=), (.=), (^.))
import           Control.Monad             (forever)
import           Control.Monad.Catch       (finally)
import           Control.Monad.Reader      (ReaderT, runReaderT)
import           Control.Monad.State       (MonadState, State, runState)
import           Control.Monad.Trans       (MonadIO (liftIO))
import           Data.Acid.Advanced        (query')
import           Data.Aeson                (FromJSON, ToJSON (toJSON),
                                            eitherDecode, encode)
import           Data.Aeson.TH             (deriveJSON, deriveToJSON)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Either.Combinators   (mapLeft)
import qualified Data.Map.Lazy             as ML
import qualified Data.Map.Strict           as M
import           Data.Maybe                (catMaybes, fromMaybe)
import qualified Data.Set                  as S
import           Data.Text                 (Text, pack)
import qualified Network.WebSockets        as WS

import           Serokell.Aeson.Options    (defaultOptions, leaveTagOptions)

import qualified RSCoin.Core               as C

import qualified RSCoin.Explorer.AcidState as DB
import           RSCoin.Explorer.Channel   (Channel, ChannelItem (..),
                                            readChannel)

-- | Run-time errors which may happen within this server.
data ServerError =
    ParseError !Text
    deriving (Show)

$(deriveJSON leaveTagOptions ''ServerError)

type ErrorableMsg msg = Either ServerError msg

-- | Communication starts with Introductory Message sent by
-- client. This type describes all such messages. Introductiory
-- message starts communication between server and client about some
-- topic (e. g. about particular address).
data IntroductoryMsg =
    -- | AddressInfo starts communication about given Address. Within
    -- this communication user can request various data about address.
    IMAddressInfo !C.Address
    deriving (Show)

$(deriveJSON defaultOptions ''IntroductoryMsg)

customDecode
    :: FromJSON a
    => BSL.ByteString -> Either ServerError a
customDecode = mapLeft (ParseError . pack) . eitherDecode

instance WS.WebSocketsData (ErrorableMsg IntroductoryMsg) where
    fromLazyByteString = customDecode
    toLazyByteString = error "Attempt to serialize IntroductoryMsg is illegal"

-- | Within communication started with AddressInfo message client can
-- send messages defined by this type.
data AddressInfoMsg
    =
      -- | GetBalance message requests balance of address associated
      -- with connection.
      AIGetBalance
    |
      -- | GetTxNumber message requests number of transactions
      -- referencing address associated with connection.
      AIGetTxNumber
    |
      -- | GetTransactions message requests transactions referencing
      -- address associated with connection. Arguments (lo, hi)
      -- determine which subset to return, i. e. transactions with
      -- indices in range [lo, hi) are returned. For instance,
      -- `AIGetTransactions (0, 2)` requests two most recent
      -- transactions.
      AIGetTransactions !(Word, Word)
    deriving (Show)

$(deriveJSON defaultOptions ''AddressInfoMsg)

instance WS.WebSocketsData (ErrorableMsg AddressInfoMsg) where
    fromLazyByteString = customDecode
    toLazyByteString = error "Attempt to serialize AddressInfoMsg is illegal"

newtype SerializableCoinsMap =
    SerializableCoinsMap C.CoinsMap
    deriving (Show)

-- | This type contains all possible messages sent by this server.
data OutcomingMsg
    =
      -- | Sent in case of error.
      OMError !ServerError
    |
      -- | Sent within `AddressInfo` session.
      OMBalance !C.PeriodId
                !SerializableCoinsMap
    |
      -- | Sent within `AddressInfo` session. Contains number of
      -- transactions referencing address over given PeriodId.
      OMTxNumber !C.PeriodId
                 !Word
    |
      -- | Sent within `AddressInfo` session. Has an indexed list of
      -- transactions referencing address over given PeriodId.
      OMTransactions !C.PeriodId ![(Word, C.Transaction)]
    deriving (Show)

mkOMBalance :: C.PeriodId -> C.CoinsMap -> OutcomingMsg
mkOMBalance pId = OMBalance pId . SerializableCoinsMap

instance ToJSON SerializableCoinsMap where
    toJSON (SerializableCoinsMap m) = toJSON . ML.assocs $ m

$(deriveToJSON defaultOptions ''OutcomingMsg)

instance WS.WebSocketsData OutcomingMsg where
    fromLazyByteString = error "Attempt to deserialize OutcomingMsg is illegal"
    toLazyByteString = encode

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

handler :: WS.PendingConnection -> ServerMonad ()
handler pendingConn = do
    conn <- liftIO $ WS.acceptRequest pendingConn
    liftIO $ WS.forkPingThread conn 30
    recv conn $ onReceive conn
  where
    onReceive conn (IMAddressInfo addr) = do
        connections <- view ssConnections
        connId <- modifyConnectionsState connections $ addConnection addr conn
        addressInfoHandler addr conn `finally`
            modifyConnectionsState connections (dropConnection addr connId)

addressInfoHandler :: C.Address -> WS.Connection -> ServerMonad ()
addressInfoHandler addr conn = forever $ recv conn onReceive
  where
    onReceive AIGetBalance =
        send conn . uncurry mkOMBalance =<<
        flip query' (DB.GetAddressBalance addr) =<< view ssDataBase
    onReceive AIGetTxNumber =
        send conn . uncurry OMTxNumber =<<
        flip query' (DB.GetAddressTxNumber addr) =<< view ssDataBase
    onReceive (AIGetTransactions indices) =
        send conn . uncurry OMTransactions =<<
        flip query' (DB.GetAddressTransactions addr indices) =<<
        view ssDataBase

sender :: Channel -> ServerMonad ()
sender channel = do
    ChannelItem{ciTransactions = txs} <- readChannel channel
    st <- view ssDataBase
    let inputs = concatMap C.txInputs txs
        outputs = concatMap C.txOutputs txs
        outputAddresses = S.fromList $ map fst outputs
        inputToAddr
            :: MonadIO m
            => C.AddrId -> m (Maybe C.Address)
        inputToAddr (txId,idx,_) =
            fmap (fst . (!! idx) . C.txOutputs) <$> query' st (DB.GetTx txId)
    affectedAddresses <-
        mappend outputAddresses . S.fromList . catMaybes <$>
        mapM inputToAddr inputs
    mapM_ notifyAboutAddressUpdate affectedAddresses

notifyAboutAddressUpdate :: C.Address -> ServerMonad ()
notifyAboutAddressUpdate addr = do
    st <- view ssDataBase
    connectionsState <- liftIO . readMVar =<< view ssConnections
    msgBalance <- uncurry mkOMBalance <$> query' st (DB.GetAddressBalance addr)
    msgTxNumber <-
        uncurry OMTxNumber <$> query' st (DB.GetAddressTxNumber addr)
    let connIds =
            fromMaybe S.empty $ connectionsState ^. csAddrToConnId . at addr
        idToConn i = connectionsState ^. csIdToConn . at i
        foldrStep connId l = maybe l (: l) $ idToConn connId
        connections = S.foldr foldrStep [] connIds
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
