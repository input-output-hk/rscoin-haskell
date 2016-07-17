module App.WSConnection
       ( Action
       , WSConnection
       , wsSend
       , wsInit
       , module WS
       ) where

import Prelude

import WebSocket                   (WEBSOCKET) as WS
import WebSocket                   (WEBSOCKET, Connection(Connection), Message(Message), URL(URL), runMessageEvent, runMessage, newWebSocket)
import Debug.Trace                 (traceAnyM)

import Control.Monad.Aff           (Aff)
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Var       (($=))
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Eff.Console   (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel              (Channel, send) as S

import Data.Either                 (Either)
import Data.Generic                (class Generic)
import Data.Argonaut.Core          (fromString)
import Data.Argonaut.Printer       (printJson)

import Serokell.Aeson.Helper       (encodeJson, decodeJson)

data Action a
    = WSConnectionOpened
    | WSReceivedData (Either String a)

type WSConnection = Connection

wsInit :: forall a. Generic a => S.Channel (Action a) -> String -> Eff (ws :: WEBSOCKET, err :: EXCEPTION, console :: CONSOLE) WSConnection
wsInit chan url = do
    connection@(Connection ws) <- newWebSocket (URL url) []
    ws.onopen $= \event -> do
        traceAnyM event
        log "onopen: Connection opened"
        S.send chan WSConnectionOpened
    ws.onmessage $= \event -> do
        traceAnyM event
        let received = runMessage $ runMessageEvent event
        log "onmessage: Received"
        S.send chan <<< WSReceivedData <<< decodeJson $ fromString received
    pure connection

wsSend :: forall a. Generic a => Connection -> a -> Aff (ws :: WEBSOCKET, err :: EXCEPTION, console :: CONSOLE) Unit
wsSend (Connection ws) value = liftEff do
    traceAnyM value
    log "onsend: Send message"
    ws.send <<< Message <<< printJson $ encodeJson value
