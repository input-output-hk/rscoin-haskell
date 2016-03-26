{-# LANGUAGE TemplateHaskell #-}
module RSCoin.Core.Protocol
       ( BankReq (..)
       , BankRes (..)
       ) where

import           Network.JsonRpc   (FromRequest (parseParams), ToRequest (..),
                                    FromResponse (parseResult), Ver (V2),
                                    Respond, JsonRpcT, jsonRpcTcpServer,
                                    receiveBatchRequest, BatchRequest (..),
                                    sendResponse, buildResponse,
                                    BatchResponse (..), sendBatchResponse)

import           Control.Monad      (forM, liftM)
import           Control.Monad.Logger (logDebug, MonadLoggerIO, runStderrLoggingT)
import           Data.Aeson        (FromJSON (parseJSON), ToJSON (toJSON))
import           Data.Aeson.Types  (emptyArray)
import           Data.Conduit.Network (serverSettings)
import           Data.Foldable     (forM_)
import           Data.Maybe        (catMaybes)

import           RSCoin.Core.Types (PeriodId, PeriodResult, Mintettes)
import           RSCoin.Core.Aeson ()

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

respond :: MonadLoggerIO m => Respond BankReq m BankRes
respond = undefined

serve :: Int -> IO ()
serve port = runStderrLoggingT $ do
    let ss = serverSettings port "::1"
    jsonRpcTcpServer V2 False ss srv

srv :: MonadLoggerIO m => JsonRpcT m ()
srv = do
    $(logDebug) "listening for new request"
    qM <- receiveBatchRequest
    case qM of
        Nothing -> do
            $(logDebug) "closed request channel, exting"
            return ()
        Just (SingleRequest q) -> do
            $(logDebug) "got request"
            rM <- buildResponse respond q
            forM_ rM sendResponse
            srv
        Just (BatchRequest qs) -> do
            $(logDebug) "got request batch"
            rs <- catMaybes `liftM` forM qs (buildResponse respond)
            sendBatchResponse $ BatchResponse rs
            srv
