module Main where

import Prelude                     (bind, pure, (<<<), (==), (||), ($), (<>), otherwise,
                                    map, const)

import App.Routes                  (match)
import App.Layout                  (view, update)
import App.Connection              (init, Action (..), WEBSOCKET) as C
import App.Types                   (Action(PageView, SocketAction, UpdateClock), State)
import App.Config                  (wsUrl, isAdmin)
import App.Common.Html             (wsSupport)

import Control.Bind                ((=<<))
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Console   (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now       (NOW, nowDateTime)
import Control.Comonad             (extract)
import DOM                         (DOM)

import Data.Maybe                  (Maybe (..), fromMaybe)
import Data.Array                  (singleton)
import Data.Functor                ((<$>))
import Data.I18N                   (detectLanguage)

import Pux                         (App, Config, CoreEffects, renderToDOM,
                                    start, EffModel, noEffects)
import Pux.Devtool                 (Action, start) as Pux.Devtool
import Pux.Router                  (sampleUrl)

import Signal                      ((~>))
import Signal.Time                 (every, second)
import Signal.Channel              (channel, CHANNEL, subscribe)

type AppEffects = (console :: CONSOLE, ws :: C.WEBSOCKET, dom :: DOM, now :: NOW)

maybeWaitSocket :: (Action -> State -> EffModel State Action AppEffects) -> Action -> State -> EffModel State Action AppEffects
maybeWaitSocket update action@(SocketAction C.ConnectionOpened) state =
    let effModel = update action $ state { socketReady = true }
    in
        { state: effModel.state { pendingActions = [] }
        , effects: effModel.effects <> map pure effModel.state.pendingActions
        }
maybeWaitSocket update action state
    | state.socketReady = update action state
    | otherwise = noEffects $ state { pendingActions = state.pendingActions <> singleton action }

-- | App configuration
-- TODO: use AppEffects also here!
config :: forall eff. State -> Eff (channel :: CHANNEL, console :: CONSOLE, err :: EXCEPTION, ws :: C.WEBSOCKET, dom :: DOM, now :: NOW | eff) (Config State Action AppEffects)
config state = do
    -- | Create a signal of URL changes.
    urlSignal <- sampleUrl
    -- | Map a signal of URL changes to PageView actions.
    let routeSignal = urlSignal ~> PageView <<< match
    -- TODO: consider putting every minute instead of secunde
    let clockSignal = every second ~> const UpdateClock

    -- FIXME: C.init is blocking and whole application is waiting for
    -- socket connection. Do this async if possible or at least show Loading intro page
    wsInput <- channel C.ConnectionClosed
    wsOn <- wsSupport
    socket <- if wsOn
        then map Just $ C.init wsInput =<< wsUrl
        else pure Nothing
    let wsSignal = subscribe wsInput ~> SocketAction
    dt <- extract <$> nowDateTime
    detectedLang <- detectLanguage
    admin <- isAdmin
    pure
        { initialState: state { socket = socket, now = dt, language = fromMaybe state.language detectedLang, isAdmin = admin, wsSupport = wsOn }
        , update: maybeWaitSocket update
        , view: view
        , inputs: [clockSignal, wsSignal, routeSignal]
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
