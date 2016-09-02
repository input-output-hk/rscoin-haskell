{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Servant part of Explorer Web Server.

module RSCoin.Explorer.Web.Servant
       ( servantApp
       ) where

import           Control.Monad.Catch       (Exception, catch, throwM)
import           Control.Monad.Except      (throwError)
import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.Trans       (liftIO)
import           Data.Acid                 (EventResult, EventState, QueryEvent)
import           Data.Typeable             (Typeable)
import           Network.Wai               (Application)
import           Servant                   ((:<|>) ((:<|>)), (:>), (:~>) (Nat),
                                            Capture,
                                            FromHttpApiData (parseUrlPiece),
                                            Get, Handler, JSON, Proxy (Proxy),
                                            ServerT, enter, err404,
                                            parseUrlPiece, serve)

import qualified RSCoin.Core               as C

import qualified RSCoin.Explorer.AcidState as DB
import           RSCoin.Explorer.Extended  (HBlockExtension,
                                            TransactionExtended)
import qualified RSCoin.Explorer.Storage   as ES
import           RSCoin.Explorer.Web.Aeson ()

type ExplorerApi =
          "tx" :> Capture "txid" C.TransactionId :> Get '[JSON] TransactionExtended
    :<|> "blockchainHeight" :> Get '[JSON] C.PeriodId
    :<|> "hBlockMetadata" :> Capture "periodId" C.PeriodId :> Get '[JSON] HBlockExtension

explorerApi :: Proxy ExplorerApi
explorerApi = Proxy

instance FromHttpApiData (C.Hash a) where
    parseUrlPiece = C.parseHash

data WebError =
    NotFound
    deriving (Show,Typeable)

instance Exception WebError

type MyHandler = ReaderT DB.State IO

convertHandler :: forall a . DB.State -> MyHandler a -> Handler a
convertHandler st act = do
    liftIO (runReaderT act st) `catch` catcher
  where
    catcher :: WebError -> Handler a
    catcher NotFound = throwError err404

query
    :: (EventState event ~ ES.Storage, QueryEvent event)
    => event -> MyHandler (EventResult event)
query event = flip DB.query event =<< ask

handleGetTx :: C.TransactionId -> MyHandler TransactionExtended
handleGetTx i = maybe (throwM NotFound) pure =<< query (DB.GetTxExtended i)

handleGetBlockchainHeight :: MyHandler C.PeriodId
handleGetBlockchainHeight = pred <$> query DB.GetExpectedPeriodId

handleGetHBlockMetadata :: C.PeriodId -> MyHandler HBlockExtension
handleGetHBlockMetadata pId =
    maybe (throwM NotFound) (pure . C.wmMetadata) =<<
    query (DB.GetHBlockExtended pId)

servantServer :: ServerT ExplorerApi MyHandler
servantServer =
    handleGetTx :<|> handleGetBlockchainHeight :<|> handleGetHBlockMetadata

servantApp :: DB.State -> Application
servantApp st = serve explorerApi $ enter nat servantServer
  where
    nat :: MyHandler :~> Handler
    nat = Nat $ convertHandler st
