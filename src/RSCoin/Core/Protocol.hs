{-# LANGUAGE TemplateHaskell #-}
module RSCoin.Core.Protocol
       ( BankReq (..)
       , BankRes (..)
       , Handler
       , serve
       , call
       , callBatch
       ) where

import           Network.JsonRpc      (FromRequest (parseParams), ToRequest (..),
                                       FromResponse (parseResult), Ver (V2),
                                       Respond, JsonRpcT, jsonRpcTcpServer,
                                       receiveBatchRequest, BatchRequest (..),
                                       sendResponse, buildResponse,
                                       BatchResponse (..), sendBatchResponse,
                                       jsonRpcTcpClient, ErrorObj, fromError,
                                       sendRequest, sendBatchRequest)

import           Control.Monad        (forM, liftM)
import           Control.Monad.Trans  (lift)
import           Control.Monad.Logger (logDebug, MonadLoggerIO, runStderrLoggingT, LoggingT)
import           Data.Aeson           (FromJSON (parseJSON), ToJSON (toJSON))
import           Data.Aeson.Types     (emptyArray)
import           Data.Conduit.Network (serverSettings, clientSettings)
import           Data.Foldable        (forM_)
import           Data.Maybe           (catMaybes)

import           RSCoin.Core.Types    (PeriodId, PeriodResult, Mintettes)
import           RSCoin.Core.Aeson    ()

data BankReq
    = ReqPeriodFinished PeriodId
    | ReqGetMintettes

instance FromRequest BankReq where
    parseParams "periodFinished" =
        Just $ fmap ReqPeriodFinished . parseJSON
    parseParams "getMintettes" =
        Just $ const $ return ReqGetMintettes
    parseParams _ =
        Nothing

instance ToRequest BankReq where
    requestMethod (ReqPeriodFinished _) = "periodFinished"
    requestMethod ReqGetMintettes = "getMintettes"
    requestIsNotif = const False

instance ToJSON BankReq where
    toJSON (ReqPeriodFinished pid) = toJSON pid
    toJSON ReqGetMintettes = emptyArray

data BankRes
    = ResPeriodFinished PeriodResult
    | ResGetMintettes Mintettes

instance FromResponse BankRes where
    parseResult "periodFinished" =
        Just $ fmap ResPeriodFinished . parseJSON
    parseResult "getMintettes" =
        Just $ fmap ResGetMintettes . parseJSON

instance ToJSON BankRes where
    toJSON (ResPeriodFinished pr) = toJSON pr
    toJSON (ResGetMintettes ms) = toJSON ms

type Handler a b = Respond a (LoggingT IO) b

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

-- FIXME: fix error handling
handleResponse :: Maybe (Either ErrorObj r) -> r
handleResponse t =
    case t of
        Nothing -> error "could not receive or parse response"
        Just (Left e) -> error $ fromError e
        Just (Right r) -> r

-- TODO: improve logging
call :: (ToRequest a, ToJSON a, FromResponse b) => Int -> a -> IO b
call port req = initCall port $ do
	$(logDebug) "send a request"
	handleResponse <$> sendRequest req

callBatch :: (ToRequest a, ToJSON a, FromResponse b) => Int -> [a] -> IO [b]
callBatch port reqs = initCall port $ do
	$(logDebug) "send a batch request"
	map handleResponse <$> sendBatchRequest reqs

initCall :: Int -> JsonRpcT (LoggingT IO) a -> IO a
initCall port action = runStderrLoggingT $
    jsonRpcTcpClient V2 True (clientSettings port "::1") action
