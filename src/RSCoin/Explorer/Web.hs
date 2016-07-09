{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

-- | Web part of Explorer.

module RSCoin.Explorer.Web
       ( application
       ) where

import           Control.Monad                  (forever)
import           Control.Monad.Catch            (Exception, catch, throwM)
import           Control.Monad.Except           (throwError)
import           Control.Monad.Reader           (ReaderT, ask, runReaderT)
import           Control.Monad.Trans            (liftIO)
import           Data.Acid.Advanced             (query')
import           Data.Aeson                     (ToJSON (toJSON), eitherDecode,
                                                 encode)
import           Data.Aeson.TH                  (deriveFromJSON, deriveToJSON)
import           Data.Either.Combinators        (mapLeft)
import qualified Data.Map                       as M
import           Data.Text                      (Text, pack)
import           Data.Typeable                  (Typeable)
import           Network.Wai                    (Application)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS
import           Servant                        ((:>), (:~>) (Nat), Capture, FromHttpApiData (parseUrlPiece),
                                                 Get, Handler, JSON,
                                                 Proxy (Proxy), ServerT, enter,
                                                 err404, parseUrlPiece, serve)

import           Serokell.Aeson.Options         (defaultOptions,
                                                 leaveTagOptions)

import qualified RSCoin.Core                    as C

import           RSCoin.Explorer.AcidState      (GetAddressCoins (..),
                                                 GetTx (..), State)

type ExplorerApi =
    "tx" :> Capture "txid" C.TransactionId :> Get '[JSON] C.Transaction

explorerApi :: Proxy ExplorerApi
explorerApi = Proxy

instance FromHttpApiData C.Hash where
    parseUrlPiece = C.parseHash

data WebError =
    NotFound
    deriving (Show,Typeable)

instance Exception WebError

type MyHandler = ReaderT State IO

convertHandler :: forall a . State -> MyHandler a -> Handler a
convertHandler st act = do
    liftIO (runReaderT act st) `catch` catcher
  where
    catcher :: WebError -> Handler a
    catcher NotFound = throwError err404

handleGetTx :: C.TransactionId -> MyHandler C.Transaction
handleGetTx i = maybe (throwM NotFound) pure =<< flip query' (GetTx i) =<< ask

servantServer :: ServerT ExplorerApi MyHandler
servantServer = handleGetTx

servantApp :: State -> Application
servantApp st = serve explorerApi $ enter nat servantServer
  where
    nat :: MyHandler :~> Handler
    nat = Nat $ convertHandler st

data IncomingMsg
    = IMGetAddressInfo C.Address
    | IMGetBalance
    deriving (Show)

$(deriveFromJSON defaultOptions ''IncomingMsg)

instance WS.WebSocketsData (Either ErrorMsg IncomingMsg) where
    fromLazyByteString = mapLeft (ParseError . pack) . eitherDecode
    toLazyByteString = undefined

data ErrorMsg
    = ParseError Text
    | UnexpectedMessage Text
    deriving (Show)

$(deriveToJSON leaveTagOptions ''ErrorMsg)

instance WS.WebSocketsData ErrorMsg where
    fromLazyByteString = undefined
    toLazyByteString = encode

data OutcomingMsg =
    OMBalance C.CoinsMap
    deriving (Show)

instance ToJSON C.CoinsMap where
    toJSON = toJSON . M.assocs

$(deriveToJSON leaveTagOptions ''OutcomingMsg)

instance WS.WebSocketsData OutcomingMsg where
    fromLazyByteString = undefined
    toLazyByteString = encode

wsApp :: State -> WS.ServerApp
wsApp st pendingConn = do
    conn <- WS.acceptRequest pendingConn
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    case msg of
        Left err -> WS.sendTextData conn (err :: ErrorMsg)
        Right (IMGetAddressInfo addr) -> startNegotiation st addr conn
        Right _ ->
            WS.sendTextData conn $
            UnexpectedMessage
                "Negotiation must start with GetAddressInfo message"

startNegotiation :: State -> C.Address -> WS.Connection -> IO ()
startNegotiation st addr conn =
    forever $
    do msg <- WS.receiveData conn
       case msg of
           Left err -> WS.sendTextData conn (err :: ErrorMsg)
           Right IMGetBalance ->
               WS.sendTextData conn . OMBalance =<<
               query' st (GetAddressCoins addr)
           Right _ ->
               WS.sendTextData conn $ UnexpectedMessage "Unexpected message"

application :: State -> Application
application st =
    websocketsOr WS.defaultConnectionOptions (wsApp st) (servantApp st)
