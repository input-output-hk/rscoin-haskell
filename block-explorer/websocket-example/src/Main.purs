module Main where

import Prelude

import Control.Bind ((=<<))
import Control.Monad.Eff.Var (($=), get)
import Control.Monad.Eff.Console (log)
import Debug.Trace (traceAnyM)

import WebSocket (Connection(..), Message(..), URL(..), runMessageEvent, runMessage, runURL, newWebSocket)

import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson)
import Data.Argonaut.Printer       (printJson)

import RSCoin.Explorer.Web.Sockets.Types as T
import RSCoin.Core.Primitives            as T
import Data.Types                        as T

main = do
  Connection socket <- newWebSocket (URL "ws://localhost:8000") []

  socket.onopen $= \event -> do
    traceAnyM event
    log "onopen: Connection opened"

    log <<< runURL =<< get socket.url

    log "onopen: Sending 'hello'"
    socket.send <<< Message <<< printJson <<< encodeJson <<< T.IMAddressInfo $ T.Address { getAddress: T.PublicKey "YblQ7+YCmxU/4InsOwSGH4Mm37zGjgy7CLrlWlnHdnM=" }


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
