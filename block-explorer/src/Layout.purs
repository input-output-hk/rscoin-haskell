module App.Layout where

import Prelude                   (($), map, (<<<), const)

import App.Counter               as Counter
import App.NotFound              as NotFound
import App.Routes                (Route(Home, NotFound))
import App.Connection            (Connection, Action (..), WEBSOCKET) as C
import App.RSCoin                (emptyAddress, Address, newAddress,
                                  addressToString, IntroductoryMsg (..))

import Data.Maybe                (Maybe (..))

import Pux                       (EffModel, noEffects)
import Pux.Html                  (Html, div, h1, p, text, input, button)
import Pux.Html.Attributes       (type_, value)
import Pux.Html.Events           (onChange, onClick)

import Control.Monad.Eff.Console (CONSOLE)
import DOM                       (DOM)

data Action
    = PageView Route
    | SocketAction C.Action
    | AddressChange Address
    | Nop

type State =
    { route   :: Route
    , count   :: Counter.State
    , socket  :: Maybe C.Connection
    , address :: Address
    }

init :: State
init =
    { route:   NotFound
    , count:   Counter.init
    , socket:  Nothing
    , address: emptyAddress
    }

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ws :: C.WEBSOCKET, dom :: DOM)
update (PageView route) state = noEffects $ state { route = route }
update (SocketAction (C.ReceivedData msg)) state = noEffects state
update (SocketAction _) state = noEffects state
update (AddressChange address) state = noEffects $ state { address = address }
update Nop state = noEffects state

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "RSCoin Block Explorer" ]
    , input [ type_ "text"
            , value $ addressToString state.address
            , onChange $ AddressChange <<< newAddress <<< _.value <<< _.target
            ] []
    , button [ onClick $ const $ SocketAction <<< C.SendIntroData $ IMAddressInfo state.address
             ] [text "Search"]
    -- TODO: we will add router later
    -- , case state.route of
    --     Home -> map Child $ Counter.view state.count
    --     NotFound -> NotFound.view state
    ]
