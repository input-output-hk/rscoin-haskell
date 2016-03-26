{-# LANGUAGE TemplateHaskell #-}
module RSCoin.Core.Protocol
       ( BankReq (..)
       , BankRes (..)
       ) where

import           Network.JsonRpc   (FromRequest (parseParams), ToRequest (..),
                                    FromResponse (parseResult))
import           Data.Aeson        (FromJSON (parseJSON), ToJSON (toJSON))
import           Data.Aeson.Types  (emptyArray)

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
