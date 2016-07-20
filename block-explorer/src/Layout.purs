module App.Layout where

import Prelude                     (($), map, (<<<), const, pure, bind)

import App.Counter                 as Counter
import App.NotFound                as NotFound
import App.Routes                  (Route(Home, NotFound))
import App.Connection              (Connection, Action (..), WEBSOCKET,
                                    introMessage, send) as C
import App.RSCoin                  (emptyAddress, Address, newAddress,
                                    addressToString, IntroductoryMsg (..),
                                    AddressInfoMsg (..))

import Data.Maybe                  (Maybe (..), fromJust)

import Pux                         (EffModel, noEffects, onlyEffects)
import Pux.Html                    (Html, div, h1, p, text, input, button, link)
import Pux.Html.Attributes         (type_, value, rel, href, className)
import Pux.Html.Events             (onChange, onClick)

import Control.Apply               ((*>))

import DOM                         (DOM)
import Control.Monad.Eff.Console   (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, catchException)

import Partial.Unsafe              (unsafePartial)

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
update (SocketAction (C.SendIntroData msg)) state =
    { state: state
    , effects:
        [ do
            C.introMessage socket' msg
            C.send socket' AIGetTxNumber
            C.send socket' AIGetBalance
            pure Nop
        -- FIXME: if socket isn't opened open some error page
        ]
    }
  where
    socket' = unsafePartial $ fromJust state.socket
update (SocketAction _) state = noEffects state
update (AddressChange address) state = noEffects $ state { address = address }
update Nop state = noEffects state

view :: State -> Html Action
view state =
  div
    []
    [ link
        [ rel "stylesheet"
        , type_ "text/css"
        , href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
        ]
        []
    , div []
        [
          h1 [] [ text "RSCoin Block Explorer" ]
        , input [ type_ "text"
                , value $ addressToString state.address
                , onChange $ AddressChange <<< newAddress <<< _.value <<< _.target
                ] []
        , button [ onClick $ const $ SocketAction <<< C.SendIntroData $ IMAddressInfo state.address
                 , className "btn btn-warning"
                 ] [text "Search"]
        ]
    -- TODO: we will add router later
    -- , case state.route of
    --     Home -> map Child $ Counter.view state.count
    --     NotFound -> NotFound.view state
    ]
