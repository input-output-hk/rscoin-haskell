module App.Layout where

import Prelude                     (($), map, (<<<), const, pure, bind, show,
                                    (==), negate)

import App.Routes                  (Route (..), addressUrl, homeUrl) as R
import App.Connection              (Connection, Action (..), WEBSOCKET,
                                    introMessage, send) as C
import App.RSCoin                  (emptyAddress, Address (..), newAddress,
                                    addressToString, IntroductoryMsg (..),
                                    AddressInfoMsg (..), Coin (..),
                                    TransactionSummarySerializable (..),
                                    OutcomingMsg (..), Color (..), ServerError (..))
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

import Pux                         (EffModel, noEffects, onlyEffects)
import Pux.Html                    (Html, div, h1, text, input, button, link,
                                    small, h5, span, table, tr, th, td,
                                    thead, tbody, nav, a, ul, li, form, script,
                                    strong)
import Pux.Router                  (navigateTo, link) as R
import Pux.Html.Attributes         (type_, value, rel, href, className,
                                    tabIndex, data_, title, role, aria,
                                    placeholder, src)
import Pux.Html.Events             (onChange, onClick, onKeyDown)

import Control.Apply               ((*>))

import DOM                         (DOM)
import Control.Monad.Eff.Console   (CONSOLE)
import Control.Monad.Eff.Class     (liftEff)

import Partial.Unsafe              (unsafePartial)

txNum :: Int
txNum = 15

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ws :: C.WEBSOCKET, dom :: DOM)
update (PageView route@(R.Address addr)) state =
    { state: state { route = route, address = addr }
    , effects:
        [ do
            C.introMessage socket' $ IMAddressInfo addr
            C.send socket' AIGetTxNumber
            C.send socket' AIGetBalance
            pure Nop
        -- FIXME: if socket isn't opened open some error page
        ]
    }
  where
    socket' = unsafePartial $ fromJust state.socket
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
        OMError (ParseError e) ->
            noEffects $ state { error = Just e }
        _ -> noEffects state
  where
    socket' = unsafePartial $ fromJust state.socket
update (SocketAction _) state = noEffects state
update (AddressChange address) state = noEffects $ state { address = address }
update Search state =
    onlyEffects state $
        [ liftEff $ R.navigateTo (R.addressUrl state.address) *> pure Nop
        ]
update DismissError state = noEffects $ state { error = Nothing }
update Nop state = noEffects state

-- TODO: make safe version of bootstrap like
-- https://github.com/slamdata/purescript-halogen-bootstrap/blob/master/src/Halogen/Themes/Bootstrap3.purs
view :: State -> Html Action
view state =
    div
        []
        [ link
            [ rel "stylesheet"
            , type_ "text/css"
            , href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            ]
            []
--        , script
--            [ src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js" ]
--            []
--        , script
--            [ src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" ]
--            []
        , nav
            [ className "navbar navbar-default" ]
            [ div
                [ className "container-fluid" ]
                [ div
                    [ className "navbar-header" ]
                    [ R.link R.homeUrl
                        [ className "navbar-brand"
                        ]
                        [ text "RS | COIN" ]
                    ]
                , ul
                    [ className "nav navbar-nav navbar-right" ]
                    [ li
                        [ className "dropdown" ]
                        [ a
                            [ className "dropwdown-toggle"
                            , href "#"
                            , data_ "toggle" "dropdown"
                            , aria "haspopup" "true"
                            , aria "expanded" "false"
                            ]
                            [ text "English"
                            , span
                                [ className "caret" ]
                                []
                            ]
                        , ul
                            [ className "dropdown-menu" ]
                            [ li
                                []
                                [ a
                                    [ href "#" ]
                                    [ text "English" ]
                                ]
                            ]
                        ]
                    ]
                , div
                    [ className "col-xs-6 navbar-form navbar-right" ]
                    [ div
                        [ className "input-group" ]
                        [ input
                            [ type_ "text"
                            , value $ addressToString state.address
                            , onChange $ AddressChange <<< newAddress <<< _.value <<< _.target
                            , onKeyDown $ \e -> if e.keyCode == 13 then Search else Nop
                            , className "form-control"
                            , placeholder "Address"
                            ] []
                        , span
                            [ className "input-group-btn" ]
                            [ button
                                [ onClick $ const Search
                                , className "btn btn-danger"
                                ] [ text "Search" ]
                            ]
                        ]
                    ]
                ]
            ]
        , case state.error of
            Just e ->
                div
                    [ className "alert alert-warning alert-dismissible"
                    , role "alert"
                    ]
                    [ button
                        [ type_ "button"
                        , className "close"
                        , data_ "dismiss" "alert"
                        , aria "label" "Close"
                        , onClick $ const DismissError
                        ]
                        [ span
                            [ aria "hidden" "true" ]
                            [ text "×" ]
                        ]
                    , strong
                        []
                        [ text "Error! " ]
                    , text e
                    ]
            Nothing -> div [] []
        , div
            [ className "container-fluid" ]
            [ case state.route of
                R.Home -> Address.view Nothing state
                R.Address addr -> Address.view (Just addr) state
                R.Transaction tId -> Transaction.view tId state
                R.NotFound -> NotFound.view state
            ]
        ]
