{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | WebSockets part of Explorer Web Server.

module RSCoin.Explorer.Web.Sockets
       ( wsApp
       ) where

import           Control.Monad             (forever)
import           Data.Acid.Advanced        (query')
import           Data.Aeson                (ToJSON (toJSON), eitherDecode,
                                            encode)
import           Data.Aeson.TH             (deriveFromJSON, deriveToJSON)
import           Data.Either.Combinators   (mapLeft)
import qualified Data.Map                  as M
import           Data.Text                 (Text, pack)
import qualified Network.WebSockets        as WS

import           Serokell.Aeson.Options    (defaultOptions, leaveTagOptions)

import qualified RSCoin.Core               as C

import           RSCoin.Explorer.AcidState (GetAddressCoins (..), State)

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
