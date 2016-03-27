{-# LANGUAGE TemplateHaskell #-}
module RSCoin.Core.Protocol
       ( BankReq (..)
       , BankRes (..)
       , MintetteReq (..)
       , MintetteRes (..)
       , UserReq (..)
       , UserRes (..)
       , Handler
       , serve
       , call
       , callBatch
       ) where

import           Network.JsonRpc       (FromRequest (parseParams), ToRequest (..),
                                        FromResponse (parseResult), Ver (V2),
                                        Respond, JsonRpcT, jsonRpcTcpServer,
                                        receiveBatchRequest, BatchRequest (..),
                                        sendResponse, buildResponse,
                                        BatchResponse (..), sendBatchResponse,
                                        jsonRpcTcpClient, ErrorObj, fromError,
                                        sendRequest, sendBatchRequest)

import           Control.Monad         (forM, liftM)
import           Control.Monad.Trans   (lift)
import           Control.Monad.Logger  (logDebug, MonadLoggerIO, runStderrLoggingT, LoggingT)
import           Data.Aeson            (FromJSON (parseJSON), ToJSON (toJSON))
import           Data.Aeson.Types      (emptyArray)
import qualified Data.ByteString.Char8 as BS
import           Data.Conduit.Network  (serverSettings, clientSettings)
import           Data.Foldable         (forM_)
import           Data.Maybe            (catMaybes)

import           RSCoin.Core.Types     (PeriodId, PeriodResult, Mintettes,
                                        NewPeriodData)
import           RSCoin.Core.Aeson     ()

---- BANK data ----

-- | Request handled by Bank (probably sent by User or Mintette)
data BankReq
    = ReqGetMintettes

instance FromRequest BankReq where
    parseParams "getMintettes" =
        Just $ const $ return ReqGetMintettes
    parseParams _ =
        Nothing

instance ToRequest BankReq where
    requestMethod ReqGetMintettes = "getMintettes"
    requestIsNotif = const False

instance ToJSON BankReq where
    toJSON ReqGetMintettes = emptyArray

-- | Responses to Bank requests (probably sent by User or Mintette)
data BankRes
    = ResGetMintettes Mintettes

instance FromResponse BankRes where
    parseResult "getMintettes" =
        Just $ fmap ResGetMintettes . parseJSON
    parseResult _ =
        Nothing

instance ToJSON BankRes where
    toJSON (ResGetMintettes ms) = toJSON ms

---- BANK data ----

---- MINTETTE data ----

-- | Request handled by Mintette (probably sent by User or Bank)
data MintetteReq
    = ReqPeriodFinished PeriodId
    | ReqAnnounceNewPeriod NewPeriodData

instance FromRequest MintetteReq where
    parseParams "periodFinished" =
        Just $ fmap ReqPeriodFinished . parseJSON
    parseParams "announceNewPeriod" =
        Just $ fmap ReqAnnounceNewPeriod . parseJSON
    parseParams _ =
        Nothing

instance ToRequest MintetteReq where
    requestMethod (ReqPeriodFinished _) = "periodFinished"
    requestMethod (ReqAnnounceNewPeriod _) = "announceNewPeriod"
    requestIsNotif = const False

instance ToJSON MintetteReq where
    toJSON (ReqPeriodFinished pid) = toJSON pid
    toJSON (ReqAnnounceNewPeriod npd) = toJSON npd

-- | Responses to Mintette requests (probably sent by User or Bank)
data MintetteRes
    = ResPeriodFinished PeriodResult
    | ResAnnounceNewPeriod

instance FromResponse MintetteRes where
    parseResult "periodFinished" =
        Just $ fmap ResPeriodFinished . parseJSON
    parseResult "announceNewPeriod" =
        Just $ const $ return ResAnnounceNewPeriod
    parseResult _ =
        Nothing

instance ToJSON MintetteRes where
    toJSON (ResPeriodFinished pid) = toJSON pid
    toJSON ResAnnounceNewPeriod = emptyArray

---- MINTETTE data ----

---- USER data ----

-- | Request handled by User (probably sent by Mintette or Bank)
data UserReq
    = ReqDummy

instance FromRequest UserReq where
    parseParams "dummy" =
        Just $ const $ return ReqDummy
    parseParams _ =
        Nothing

instance ToRequest UserReq where
    requestMethod ReqDummy = "dummy"
    requestIsNotif = const False

instance ToJSON UserReq where
    toJSON ReqDummy = emptyArray

-- | Responses to User requests (probably sent by Mintette or Bank)
data UserRes
    = ResDummy

instance FromResponse UserRes where
    parseResult "dummy" =
        Just $ const $ return ResDummy
    parseResult _ =
        Nothing

instance ToJSON UserRes where
    toJSON ResDummy = emptyArray

---- USER data ----

-- | Handler is a function that takes request (ie BankReq) and returns
-- a response (ie BankRes). Ment to be used in servers. 
type Handler a b = Respond a (LoggingT IO) b

-- | Runs a TCP server transport for JSON-RPC.
serve :: (FromRequest a, ToJSON b) => Int -> Handler a b -> IO ()
serve port handler = runStderrLoggingT $ do
    let ss = serverSettings port "::1"
    jsonRpcTcpServer V2 False ss $ srv handler

srv :: (MonadLoggerIO m, FromRequest a, ToJSON b) => Respond a m b -> JsonRpcT m ()
srv handler = do
    $(logDebug) "listening for new request"
    qM <- receiveBatchRequest
    case qM of
        Nothing -> do
            $(logDebug) "closed request channel, exting"
            return ()
        Just (SingleRequest q) -> do
            $(logDebug) "got request"
            rM <- lift $ buildResponse handler q
            forM_ rM sendResponse
            srv handler
        Just (BatchRequest qs) -> do
            $(logDebug) "got request batch"
            rs <- lift $ catMaybes `liftM` forM qs (buildResponse handler)
            sendBatchResponse $ BatchResponse rs
            srv handler

-- TODO: fix error handling
handleResponse :: Maybe (Either ErrorObj r) -> r
handleResponse t =
    case t of
        Nothing -> error "could not receive or parse response"
        Just (Left e) -> error $ fromError e
        Just (Right r) -> r

-- TODO: improve logging
-- | Send a request.
call :: (ToRequest a, ToJSON a, FromResponse b) => Int -> String -> a -> IO b
call port host req = initCall port host $ do
    $(logDebug) "send a request"
    handleResponse <$> sendRequest req

-- | Send multiple requests in a batch.
callBatch :: (ToRequest a, ToJSON a, FromResponse b) => Int -> String -> [a] -> IO [b]
callBatch port host reqs = initCall port host $ do
    $(logDebug) "send a batch request"
    map handleResponse <$> sendBatchRequest reqs

-- | Runs a TCP client transport for JSON-RPC.
initCall :: Int -> String -> JsonRpcT (LoggingT IO) a -> IO a
initCall port host action = runStderrLoggingT $
    jsonRpcTcpClient V2 True (clientSettings port $ BS.pack host) action
