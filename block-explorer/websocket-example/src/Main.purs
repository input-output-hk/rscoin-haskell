module Main where

import Prelude

import Control.Bind ((=<<))
import Control.Monad.Eff.Var (($=), get)
import Control.Monad.Eff.Console (log)
import Debug.Trace (traceAnyM)

import WebSocket (Connection(..), Message(..), URL(..), runMessageEvent, runMessage, runURL, newWebSocket)

main = do
  Connection socket <- newWebSocket (URL "ws://echo.websocket.org") []

  socket.onopen $= \event -> do
    traceAnyM event
    log "onopen: Connection opened"

    log <<< runURL =<< get socket.url

    log "onopen: Sending 'hello'"
    socket.send (Message "hello")

    log "onopen: Sending 'goodbye'"
    socket.send (Message "goodbye")

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
