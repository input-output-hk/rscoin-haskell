{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Servant part of Explorer Web Server.

module RSCoin.Explorer.Web.Servant
       ( servantApp
       ) where

import           Control.Monad.Catch               (Exception, catch, throwM)
import           Control.Monad.Except              (throwError)
import           Control.Monad.Reader              (ReaderT, ask, runReaderT)
import           Control.Monad.Trans               (liftIO)
import           Data.Typeable                     (Typeable)
import           Network.Wai                       (Application)
import           Servant                           ((:>), (:~>) (Nat), Capture, FromHttpApiData (parseUrlPiece),
                                                    Get, Handler, JSON,
                                                    Proxy (Proxy), ServerT,
                                                    enter, err404,
                                                    parseUrlPiece, serve)

import qualified RSCoin.Core                       as C

import           RSCoin.Explorer.AcidState         (GetTx (..), State, query)
import           RSCoin.Explorer.Web.Sockets.Types (TransactionSummarySerializable,
                                                    mkTransactionSummarySerializable)

type ExplorerApi =
    "tx" :> Capture "txid" C.TransactionId :> Get '[JSON] TransactionSummarySerializable

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

handleGetTx :: C.TransactionId -> MyHandler TransactionSummarySerializable
handleGetTx i =
    maybe (throwM NotFound) (pure . mkTransactionSummarySerializable) =<<
    flip query (GetTx i) =<< ask

servantServer :: ServerT ExplorerApi MyHandler
servantServer = handleGetTx

servantApp :: State -> Application
servantApp st = serve explorerApi $ enter nat servantServer
  where
    nat :: MyHandler :~> Handler
    nat = Nat $ convertHandler st
