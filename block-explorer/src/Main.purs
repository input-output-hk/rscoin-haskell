module Main where

import Prelude                     (bind, pure, (<<<))

import App.Routes                  (match)
import App.Layout                  (Action(PageView, WSAction), State, view, update)
import App.WSConnection            (wsInit, Action (..), WEBSOCKET) as WS

import Control.Bind                ((=<<))
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Console   (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM                         (DOM)

import Data.Maybe                  (Maybe (..))

import Pux                         (App, Config, CoreEffects, renderToDOM, start)
import Pux.Devtool                 (Action, start) as Pux.Devtool
import Pux.Router                  (sampleUrl)

import Signal                      ((~>))
import Signal.Channel              (channel, CHANNEL, subscribe)

type AppEffects = (console :: CONSOLE, ws :: WS.WEBSOCKET, dom :: DOM)

-- | App configuration
-- TODO: use AppEffects also here!
config :: forall eff. State -> Eff (channel :: CHANNEL, console :: CONSOLE, err :: EXCEPTION, ws :: WS.WEBSOCKET, dom :: DOM | eff) (Config State Action AppEffects)
config state = do
    -- | Create a signal of URL changes.
    urlSignal <- sampleUrl
    -- | Map a signal of URL changes to PageView actions.
    let routeSignal = urlSignal ~> PageView <<< match
    wsInput <- channel WS.WSConnectionClosed
    socket <- WS.wsInit wsInput "ws://localhost:8000"
    let wsSignal = subscribe wsInput ~> WSAction
    pure
        { initialState: state { socket = Just socket }
        , update: update
        , view: view
        , inputs: [wsSignal, routeSignal]
        }

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
    app <- start =<< config state
    renderToDOM "#app" app.html
    -- | Used by hot-reloading code in support/index.js
    pure app

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
    app <- Pux.Devtool.start =<< config state
    renderToDOM "#app" app.html
    -- | Used by hot-reloading code in support/index.js
    pure app
