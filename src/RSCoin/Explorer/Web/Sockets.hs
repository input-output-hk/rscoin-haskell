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

import           Control.Concurrent.MVar   (MVar, modifyMVar, newMVar)
import           Control.Lens              (at, makeLenses, use, view, (%=),
                                            (+=), (.=))
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
import qualified Data.Set                  as S
import           Data.Text                 (Text, pack)
import qualified Network.WebSockets        as WS

import           Serokell.Aeson.Options    (defaultOptions, leaveTagOptions)

import qualified RSCoin.Core               as C

import qualified RSCoin.Explorer.AcidState as DB
import           RSCoin.Explorer.Channel   (Channel)

-- | Run-time errors which may happen within this server.
data ServerError =
    ParseError Text
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
    IMAddressInfo C.Address
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
data AddressInfoMsg =
    -- | GetBalance message requests balance of address associated
    -- with connection.
    AIGetBalance
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
      OMError ServerError
    |
      -- | Sent in response to `AddressInfo` message.
      OMBalance SerializableCoinsMap
    deriving (Show)

mkOMBalance :: C.CoinsMap -> OutcomingMsg
mkOMBalance = OMBalance . SerializableCoinsMap

instance ToJSON SerializableCoinsMap where
    toJSON (SerializableCoinsMap m) = toJSON . ML.assocs $ m

$(deriveToJSON defaultOptions ''OutcomingMsg)

instance WS.WebSocketsData OutcomingMsg where
    fromLazyByteString = error "Attempt to deserialize OutcomingMsg is illegal"
    toLazyByteString = encode

type ConnectionId = Word

data ConnectionsState = ConnectionsState
    { _csCounter     :: Word
    , _csIds         :: M.Map ConnectionId WS.Connection
    , _csConnections :: M.Map C.Address (S.Set ConnectionId)
    }

$(makeLenses ''ConnectionsState)

addConnection
    :: MonadState ConnectionsState m
    => C.Address -> WS.Connection -> m ConnectionId
addConnection addr conn = do
    i <- use csCounter
    csCounter += 1
    csIds . at i .= Just conn
    csConnections . at addr %= Just . (maybe (S.singleton i) (S.insert i))
    return i

dropConnection
    :: MonadState ConnectionsState m
    => C.Address -> ConnectionId -> m ()
dropConnection addr connId = do
    csIds . at connId .= Nothing
    csConnections . at addr %= fmap (S.delete connId)

type ConnectionsVar = MVar ConnectionsState

data ServerState = ServerState
    { _ssDataBase    :: DB.State
    , _ssConnections :: ConnectionsVar
    }

mkConnectionsState :: ConnectionsState
mkConnectionsState =
    ConnectionsState
    { _csCounter = 0
    , _csIds = M.empty
    , _csConnections = M.empty
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
        send conn . mkOMBalance =<<
        flip query' (DB.GetAddressCoins addr) =<< view ssDataBase

-- | Given access to Explorer's data base and channel, returns
-- WebSockets server application.
mkWsApp
    :: MonadIO m
    => Channel -> DB.State -> m WS.ServerApp
mkWsApp _ st =
    liftIO $
    do connections <- newMVar mkConnectionsState
       let ss =
               ServerState
               { _ssDataBase = st
               , _ssConnections = connections
               }
           app pc = runReaderT (handler pc) ss
       return app
