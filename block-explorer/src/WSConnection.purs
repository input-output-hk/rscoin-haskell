module App.WSConnection
       ( Action (..)
       , WSConnection
       , wsSend
       , wsInit
       , wsIntroMessage
       , module WS
       ) where

import Prelude

import WebSocket                         (WEBSOCKET) as WS
import WebSocket                         (WEBSOCKET, Connection(Connection), Message(Message), URL(URL), runMessageEvent, runMessage, newWebSocket)
import Debug.Trace                       (traceAnyM)

import Control.Monad.Aff                 (Aff)
import Control.Monad.Eff                 (Eff)
import Control.Monad.Eff.Var             (($=))
import Control.Monad.Eff.Class           (liftEff)
import Control.Monad.Eff.Console         (CONSOLE, log)
import Control.Monad.Eff.Exception       (EXCEPTION)
import Signal.Channel                    (Channel, send) as S

import Data.Either                       (Either)
import Data.Argonaut.Core                (fromString)
import Data.Argonaut.Printer             (printJson)

import Serokell.Aeson.Helper             (encodeJson, decodeJson)

import RSCoin.Explorer.Web.Sockets.Types (OutcomingMsg, AddressInfoMsg, IntroductoryMsg)

data Action
    = WSConnectionOpened
    | WSReceivedData (Either String OutcomingMsg)
    | WSConnectionClosed

type WSConnection = Connection

wsInit :: forall eff. S.Channel Action -> String -> Eff (ws :: WEBSOCKET, err :: EXCEPTION, console :: CONSOLE | eff) WSConnection
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

wsIntroMessage :: forall eff. Connection -> IntroductoryMsg -> Aff (ws :: WEBSOCKET, err :: EXCEPTION, console :: CONSOLE | eff) Unit
wsIntroMessage (Connection ws) value = liftEff do
    traceAnyM value
    log "onsend: Send introductory message"
    ws.send <<< Message <<< printJson $ encodeJson value

wsSend :: forall eff. Connection -> AddressInfoMsg -> Aff (ws :: WEBSOCKET, err :: EXCEPTION, console :: CONSOLE | eff) Unit
wsSend (Connection ws) value = liftEff do
    traceAnyM value
    log "onsend: Send message"
    ws.send <<< Message <<< printJson $ encodeJson value
