module App.Layout where

import Prelude                     (($), map, (<<<), const, pure, bind, show,
                                    (==))


import App.Counter                 as Counter
-- import App.NotFound                as NotFound
import App.Routes                  (Route(NotFound))
import App.Connection              (Connection, Action (..), WEBSOCKET,
                                    introMessage, send) as C
import App.RSCoin                  (emptyAddress, Address, newAddress,
                                    addressToString, IntroductoryMsg (..),
                                    AddressInfoMsg (..), Coin (..),
                                    Transaction (..), OutcomingMsg (..))

import Data.Maybe                  (Maybe (..), fromJust)
import Data.Tuple                  (Tuple (..), snd)
import Data.Tuple.Nested           (uncurry2, uncurry3)
import Data.Either                 (fromRight)
import Data.Array                  (concatMap)

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

data Action
    = PageView Route
    | SocketAction C.Action
    | AddressChange Address
    | Nop

type State =
    { route        :: Route
    , count        :: Counter.State
    , socket       :: Maybe C.Connection
    , address      :: Address
    , balance      :: Array (Tuple Int Coin)
    , transactions :: Array Transaction
    , periodId     :: Int
    }

init :: State
init =
    { route:        NotFound
    , count:        Counter.init
    , socket:       Nothing
    , address:      emptyAddress
    , balance:      []
    , transactions: []
    , periodId:          0
    }

txNum :: Int
txNum = 15

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ws :: C.WEBSOCKET, dom :: DOM)
update (PageView route) state = noEffects $ state { route = route }
update (SocketAction (C.ReceivedData msg)) state =
    case unsafePartial $ fromRight msg of
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
    , div []
        [
          div
            [ className "page-header" ]
            [ h1 [] [ text "RSCoin "
                    , small [] [text "blockchain" ]
                    ]
            ]
        , div
            [ className "row navbar" ]
            [ div
                [ className "col-xs-3" ]
                [ h5 [] [ text "RSCoin" ]
                ]
            , div
                [ className "col-xs-5" ]
                [ input
                    [ type_ "text"
                    , value $ addressToString state.address
                    , onChange $ AddressChange <<< newAddress <<< _.value <<< _.target
                    , onKeyDown $ \e -> if e.keyCode == 13 then clickSearch else Nop
                    , className "form-control"
                    ] []
                ]
            , div
                [ className "col-xs-1" ]
                [
                  button
                    [ onClick $ const clickSearch
                    , className "btn btn-danger"
                    ] [text "Search"]
                ]
            , div
                [ className "col-xs-2 col-xs-offset-1 text-right" ]
                [ text "English"
                , span [ className "caret" ] []
                ]
            ]
        , div
            [ className "col-xs-6" ]
            [ div
                [ className "panel panel-default" ]
                [ div
                    [ className "panel-heading" ]
                    [ text "Balance" ]
                , table
                    [ className "table table-striped table-hover" ]
                    [ thead [] [ tr []
                        [ th [] [ text "Height" ]
                        , th [] [ text "Coin color" ]
                        , th [] [ text "Coin amount" ]
                        ]]
                    , tbody [] $ map (uncurry2 $ coinRow state.periodId) state.balance
                    ]
                ]
            , div
                [ className "panel panel-default" ]
                [ div
                    [ className "panel-heading" ]
                    [ text "Transaction output feed" ]
                , table
                    [ className "table table-striped table-hover" ]
                    [ thead [] [ tr []
                        [ th [] [ text "Address to" ]
                        , th [] [ text "Coin color" ]
                        , th [] [ text "Coin amount" ]
                        ]]
                    , tbody [] <<< map (uncurry2 txOutputRow) $ concatMap (\(Transaction t) -> t.txOutputs) state.transactions
                    ]
                ]
            ]
        , div
            [ className "col-xs-6" ]
            [ div
                [ className "panel panel-default" ]
                [ div
                    [ className "panel-heading" ]
                    [ text "Transaction input feed" ]
                , table
                    [ className "table table-striped table-hover" ]
                    [ thead [] [ tr []
                        [ th [] [ text "Transaction hash from" ]
                        , th [] [ text "Address index" ]
                        , th [] [ text "Coin color" ]
                        , th [] [ text "Coin amount" ]
                        ]]
                    , tbody [] <<< map (uncurry3 txInputRow) $ concatMap (\(Transaction t) -> t.txInputs) state.transactions
                    ]
                ]
            ]
        ]
    -- TODO: we will add router later
    -- , case state.route of
    --     Home -> map Child $ Counter.view state.count
    --     NotFound -> NotFound.view state
    ]
  where
    coinRow pid _ (Coin c) =
        tr []
           [ td [] [ text $ show pid ]
           , td [] [ text $ show c.getColor ]
           , td [] [ text $ show c.getCoin ]
           ]
    txInputRow h i (Coin c) =
        tr []
           [ td [] [ text $ show h ]
           , td [] [ text $ show i ]
           , td [] [ text $ show c.getColor ]
           , td [] [ text $ show c.getCoin ]
           ]
    txOutputRow adr (Coin c) =
        tr []
           [ td [] [ text $ addressToString adr ]
           , td [] [ text $ show c.getColor ]
           , td [] [ text $ show c.getCoin ]
           ]
    clickSearch = SocketAction <<< C.SendIntroData $ IMAddressInfo state.address
