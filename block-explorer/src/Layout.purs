module App.Layout where

import Prelude                   (($), map)

import App.Counter               as Counter
import App.NotFound              as NotFound
import App.Routes                (Route(Home, NotFound))
import App.WSConnection          (WSConnection, Action, WEBSOCKET) as WS
import Data.Maybe                (Maybe (..))

import Pux                       (EffModel, noEffects)
import Pux.Html                  (Html, div, h1, p, text)

import Control.Monad.Eff.Console (CONSOLE)
import DOM                       (DOM)

data Action
    = Child Counter.Action
    | PageView Route
    | WSAction WS.Action
    | Nop

type State =
    { route  :: Route
    , count  :: Counter.State
    , socket :: Maybe WS.WSConnection
    }

init :: State
init =
    { route:  NotFound
    , count:  Counter.init
    , socket: Nothing
    }

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ws :: WS.WEBSOCKET, dom :: DOM)
update (PageView route) state = noEffects $ state { route = route }
update (Child action) state = noEffects $ state { count = Counter.update action state.count }
update (WSAction _) state = noEffects state
update Nop state = noEffects state

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> map Child $ Counter.view state.count
        NotFound -> NotFound.view state
    ]
