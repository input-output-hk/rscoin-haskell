{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Web part of Explorer.

module RSCoin.Explorer.Web
       ( application
       ) where

import           Control.Monad.Catch            (Exception, catch, throwM)
import           Control.Monad.Except           (throwError)
import           Control.Monad.Reader           (ReaderT, ask, runReaderT)
import           Control.Monad.Trans            (liftIO)
import           Data.Acid.Advanced             (query')
import           Data.Text                      (Text)
import           Data.Typeable                  (Typeable)
import           Network.Wai                    (Application)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS
import           Servant                        ((:>), (:~>) (Nat), Capture, FromHttpApiData (parseUrlPiece),
                                                 Get, Handler, JSON,
                                                 Proxy (Proxy), ServerT, enter,
                                                 err404, parseUrlPiece, serve)

import qualified RSCoin.Core                    as C

import           RSCoin.Explorer.AcidState      (GetTx (..), State)

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

wsApp :: WS.ServerApp
wsApp pendingConn = do
    conn <- WS.acceptRequest pendingConn
    WS.forkPingThread conn 30
    WS.sendTextData conn ("top kek" :: Text)

application :: State -> Application
application st = websocketsOr WS.defaultConnectionOptions wsApp $ servantApp st
