module App.Layout where

import Prelude          (($), map)

import App.Counter      as Counter
import App.NotFound     as NotFound
import App.Routes       (Route(Home, NotFound))
import App.WSConnection (WSConnection, Action (..)) as WS

import Data.Maybe       (Maybe (..))

import Pux.Html         (Html, div, h1, p, text)

data Action
    = Child (Counter.Action)
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

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { count = Counter.update action state.count }
update (WSAction _) state = state
update Nop state = state

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
