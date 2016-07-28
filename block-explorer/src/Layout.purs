module App.Layout where

import Prelude                     (($), map, (<<<), const, pure, bind, show,
                                    (==))

import App.Routes                  (Route (..))
import App.Connection              (Connection, Action (..), WEBSOCKET,
                                    introMessage, send) as C
import App.RSCoin                  (emptyAddress, Address, newAddress,
                                    addressToString, IntroductoryMsg (..),
                                    AddressInfoMsg (..), Coin (..),
                                    TransactionSummarySerializable (..),
                                    OutcomingMsg (..), Color (..))
import App.Types                   (Action (..), State (..))
import App.View.Address            (view) as Address
import App.View.NotFound           (view) as NotFound
import App.View.Transaction        (view) as Transaction

import Data.Maybe                  (Maybe (..), fromJust)
import Data.Tuple                  (Tuple (..), snd)
import Data.Tuple.Nested           (uncurry2)
import Data.Either                 (fromRight)
import Data.Generic                (gShow)
import Debug.Trace                 (traceAny)

import Pux                         (EffModel, noEffects)
import Pux.Html                    (Html, div, h1, text, input, button, link,
                                    small, h5, span, table, tr, th, td,
                                    thead, tbody)
import Pux.Html.Attributes         (type_, value, rel, href, className)
import Pux.Html.Events             (onChange, onClick, onKeyDown)

import Control.Apply               ((*>))

import DOM                         (DOM)
import Control.Monad.Eff.Console   (CONSOLE)

import Partial.Unsafe              (unsafePartial)

txNum :: Int
txNum = 15

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ws :: C.WEBSOCKET, dom :: DOM)
update (PageView route) state = noEffects $ state { route = route }
update (SocketAction (C.ReceivedData msg)) state = traceAny (gShow msg) $
    \_ -> case unsafePartial $ fromRight msg of
        OMBalance pid arr ->
            { state: state { balance = arr, periodId = pid }
            , effects:
                [ do
                    C.send socket' <<< AIGetTransactions $ Tuple 0 txNum
                    pure Nop
                ]
            }
        OMTransactions _ arr ->
            noEffects $ state { transactions = map snd arr }
        _ -> noEffects state
  where
    socket' = unsafePartial $ fromJust state.socket
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

-- TODO: make safe version of bootstrap like
-- https://github.com/slamdata/purescript-halogen-bootstrap/blob/master/src/Halogen/Themes/Bootstrap3.purs
view :: State -> Html Action
view state =
  div
    [ className "container-fluid" ]
    [ link
        [ rel "stylesheet"
        , type_ "text/css"
        , href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
        ]
        []
    , case state.route of
        Home -> Address.view state
        Transaction tId -> Transaction.view tId state
        NotFound -> NotFound.view state
    ]
