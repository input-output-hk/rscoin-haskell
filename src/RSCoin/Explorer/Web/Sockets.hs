{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | WebSockets part of Explorer Web Server.

module RSCoin.Explorer.Web.Sockets
       ( wsApp
       ) where

import           Control.Lens              (makeLenses, view)
import           Control.Monad             (forever)
import           Control.Monad.Reader      (ReaderT, runReaderT)
import           Control.Monad.Trans       (MonadIO (liftIO))
import           Data.Acid.Advanced        (query')
import           Data.Aeson                (FromJSON, ToJSON (toJSON),
                                            eitherDecode, encode)
import           Data.Aeson.TH             (deriveFromJSON, deriveJSON,
                                            deriveToJSON)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Either.Combinators   (mapLeft)
import qualified Data.Map                  as M
import           Data.Text                 (Text, pack)
import qualified Network.WebSockets        as WS

import           Serokell.Aeson.Options    (defaultOptions, leaveTagOptions)

import qualified RSCoin.Core               as C

import           RSCoin.Explorer.AcidState (GetAddressCoins (..), State)

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

$(deriveFromJSON defaultOptions ''IntroductoryMsg)

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

$(deriveFromJSON defaultOptions ''AddressInfoMsg)

instance WS.WebSocketsData (ErrorableMsg AddressInfoMsg) where
    fromLazyByteString = customDecode
    toLazyByteString = error "Attempt to serialize AddressInfoMsg is illegal"

-- | This type contains all possible messages sent by this server.
data OutcomingMsg
    =
      -- | Sent in case of error.
      OMError ServerError
    |
      -- | Sent in response to `AddressInfo` message.
      OMBalance C.CoinsMap
    deriving (Show)

instance ToJSON C.CoinsMap where
    toJSON = toJSON . M.assocs

$(deriveToJSON defaultOptions ''OutcomingMsg)

instance WS.WebSocketsData OutcomingMsg where
    fromLazyByteString = error "Attempt to deserialize OutcominMsg is illegal"
    toLazyByteString = encode

data ServerState = ServerState
    { _ssDataBase :: State
    }

$(makeLenses ''ServerState)

type Handler = ReaderT ServerState IO

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

handler :: WS.PendingConnection -> Handler ()
handler pendingConn = do
    conn <- liftIO $ WS.acceptRequest pendingConn
    liftIO $ WS.forkPingThread conn 30
    recv conn $ onReceive conn
  where
    onReceive conn (IMAddressInfo addr) = startNegotiation addr conn

startNegotiation :: C.Address -> WS.Connection -> Handler ()
startNegotiation addr conn = forever $ recv conn onReceive
  where
    onReceive AIGetBalance =
        send conn . OMBalance =<<
        flip query' (GetAddressCoins addr) =<< view ssDataBase

-- | Given access to Explorer's data base, returns WebSockets server
-- application.
wsApp :: State -> WS.ServerApp
wsApp st = flip runReaderT ss . handler
  where
    ss = ServerState st
