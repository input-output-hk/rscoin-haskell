module App.Layout where

import Prelude                   (($), map, (<<<))

import App.Counter               as Counter
import App.NotFound              as NotFound
import App.Routes                (Route(Home, NotFound))
import App.WSConnection          (WSConnection, Action (..), WEBSOCKET) as WS
import App.RSCoin                (emptyAddress, Address, newAddress, addressToString)

import Data.Maybe                (Maybe (..))

import Pux                       (EffModel, noEffects)
import Pux.Html                  (Html, div, h1, p, text, input)
import Pux.Html.Attributes       (type_, value)
import Pux.Html.Events           (onChange)

import Control.Monad.Eff.Console (CONSOLE)
import DOM                       (DOM)

data Action
    = PageView Route
    | WSAction WS.Action
    | AddressChange Address
    | Nop

type State =
    { route   :: Route
    , count   :: Counter.State
    , socket  :: Maybe WS.WSConnection
    , address :: Address
    }

init :: State
init =
    { route:   NotFound
    , count:   Counter.init
    , socket:  Nothing
    , address: newAddress ""
    }

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ws :: WS.WEBSOCKET, dom :: DOM)
update (PageView route) state = noEffects $ state { route = route }
update (WSAction (WS.WSReceivedData msg)) state = noEffects state
update (WSAction _) state = noEffects state
update (AddressChange address) state = noEffects $ state { address = address }
update Nop state = noEffects state

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "RSCoin Block Explorer" ]
    , input [ type_ "text", value $ addressToString state.address, onChange $ AddressChange <<< newAddress <<< _.value <<< _.target ] []
    -- TODO: we will add router later
    -- , case state.route of
    --     Home -> map Child $ Counter.view state.count
    --     NotFound -> NotFound.view state
    ]
