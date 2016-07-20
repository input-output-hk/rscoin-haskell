module App.Connection
       ( Action (..)
       , Connection
       , send
       , init
       , introMessage
       , module WS
       ) where

import Prelude

import WebSocket                   (WEBSOCKET) as WS
import WebSocket                   (WEBSOCKET, Connection(Connection), Message(Message), URL(URL), runMessageEvent, runMessage, newWebSocket) as W
import Debug.Trace                 (traceAnyM)

import Control.Bind                ((>>=))
import Control.Monad.Aff           (Aff)
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Var       (($=))
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Eff.Console   (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel              (Channel, send) as S

import Data.Either                 (Either)
import Data.Argonaut.Printer       (printJson)
import Data.Argonaut.Parser        (jsonParser)

import Serokell.Aeson.Helper       (encodeJson, decodeJson)

import App.RSCoin                  (OutcomingMsg, AddressInfoMsg, IntroductoryMsg)

data Action
    = ConnectionOpened
    | ReceivedData (Either String OutcomingMsg)
    | SendIntroData IntroductoryMsg
    | ConnectionClosed

type Connection = W.Connection

init :: forall eff. S.Channel Action -> String -> Eff (ws :: W.WEBSOCKET, err :: EXCEPTION, console :: CONSOLE | eff) W.Connection
init chan url = do
    connection@(W.Connection ws) <- W.newWebSocket (W.URL url) []
    ws.onopen $= \event -> do
        traceAnyM event
        log "onopen: Connection opened"
        S.send chan ConnectionOpened
    ws.onmessage $= \event -> do
        traceAnyM event
        let received = W.runMessage $ W.runMessageEvent event
        log "onmessage: Received"
        S.send chan <<< ReceivedData $ jsonParser received >>= decodeJson
    pure connection

introMessage :: forall eff. W.Connection -> IntroductoryMsg -> Aff (ws :: W.WEBSOCKET, err :: EXCEPTION, console :: CONSOLE | eff) Unit
introMessage (W.Connection ws) value = liftEff do
    traceAnyM value
    log "onsend: Send introductory message"
    ws.send <<< W.Message <<< printJson $ encodeJson value

send :: forall eff. W.Connection -> AddressInfoMsg -> Aff (ws :: W.WEBSOCKET, err :: EXCEPTION, console :: CONSOLE | eff) Unit
send (W.Connection ws) value = liftEff do
    traceAnyM value
    log "onsend: Send message"
    ws.send <<< W.Message <<< printJson $ encodeJson value
