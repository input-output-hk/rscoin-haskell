{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Web part of Explorer.

module RSCoin.Explorer.Web
       ( application
       ) where

import           Network.Wai (Application)
import           Servant     ((:>), Capture, FromHttpApiData (parseUrlPiece),
                              Get, Handler, JSON, Proxy (Proxy), Server,
                              parseUrlPiece, serve)

import qualified RSCoin.Core as C

type ExplorerApi =
    "tx" :> Capture "txid" C.TransactionId :> Get '[JSON] C.Transaction

explorerApi :: Proxy ExplorerApi
explorerApi = Proxy

instance FromHttpApiData C.Hash where
    parseUrlPiece = C.parseHash

handleGetTx :: C.TransactionId -> Handler C.Transaction
handleGetTx = return undefined

servantServer :: Server ExplorerApi
servantServer = handleGetTx

application :: Application
application = serve explorerApi servantServer
