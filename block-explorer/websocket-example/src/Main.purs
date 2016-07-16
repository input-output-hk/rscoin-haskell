module Main where

import Prelude

import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Var (($=), get)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Debug.Trace (traceAnyM)

import WebSocket (Connection(..), Message(..), URL(..), runMessageEvent, runMessage, runURL, newWebSocket, WEBSOCKET)

import Data.Argonaut.Printer       (printJson)

import RSCoin.Explorer.Web.Sockets.Types (IntroductoryMsg(IMAddressInfo))
import RSCoin.Core.Primitives            (Address(Address))
import Data.Types                        (PublicKey(PublicKey))
import Serokell.Aeson.Helper (encodeJson)

main :: forall e. Eff (ws :: WEBSOCKET, err :: EXCEPTION | e) Unit
main = do
  Connection socket <- newWebSocket (URL "ws://localhost:8000") []

  socket.onopen $= \event -> do
    traceAnyM event
    log "onopen: Connection opened"

    log <<< runURL =<< get socket.url

    let json = printJson <<< encodeJson <<< IMAddressInfo $ Address { getAddress: PublicKey "YblQ7+YCmxU/4InsOwSGH4Mm37zGjgy7CLrlWlnHdnM=" }
    log $ "onopen: Sending json: " <> json
    socket.send $ Message json


  socket.onmessage $= \event -> do
    traceAnyM event
    let received = runMessage (runMessageEvent event)

    log $ "onmessage: Received '" <> received <> "'"

    when (received == "goodbye") do
      log "onmessage: closing connection"
      socket.close

  socket.onclose $= \event -> do
    traceAnyM event
    log "onclose: Connection closed"
