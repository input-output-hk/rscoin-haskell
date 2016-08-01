module Main where

import Prelude                     (bind, pure, (<<<))

import App.Routes                  (match)
import App.Layout                  (view, update)
import App.Connection              (init, Action (..), WEBSOCKET) as C
import App.Types                   (Action(PageView, SocketAction), State)
import App.Config                  (wsUrl)

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

type AppEffects = (console :: CONSOLE, ws :: C.WEBSOCKET, dom :: DOM)

-- | App configuration
-- TODO: use AppEffects also here!
config :: forall eff. State -> Eff (channel :: CHANNEL, console :: CONSOLE, err :: EXCEPTION, ws :: C.WEBSOCKET, dom :: DOM | eff) (Config State Action AppEffects)
config state = do
    -- | Create a signal of URL changes.
    urlSignal <- sampleUrl
    -- | Map a signal of URL changes to PageView actions.
    let routeSignal = urlSignal ~> PageView <<< match

    -- FIXME: C.init is blocking and whole application is waiting for
    -- socket connection. Do this async if possible or at least show Loading intro page
    wsInput <- channel C.ConnectionClosed
    socket <- C.init wsInput =<< wsUrl
    let wsSignal = subscribe wsInput ~> SocketAction
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
