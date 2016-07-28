module App.View.Transaction where

import Prelude                     (($), map, (<<<), const, pure, bind, show,
                                    (==))

import App.RSCoin                  (emptyAddress, Address, newAddress,
                                    addressToString, IntroductoryMsg (..),
                                    AddressInfoMsg (..), Coin (..),
                                    TransactionSummarySerializable (..),
                                    OutcomingMsg (..), Color (..), TransactionId)
import App.Types                   (Action (..), State (..))
import App.Connection              (Action (..)) as C
import App.Routes                  (txUrl)

import Pux                         (EffModel, noEffects)
import Pux.Html                    (Html, div, h1, text, input, button,
                                    small, h5, span, table, tr, th, td,
                                    thead, tbody)
import Pux.Html.Attributes         (type_, value, rel, href, className)
import Pux.Html.Events             (onChange, onClick, onKeyDown)
import Pux.Router                  (link)

import Data.Tuple.Nested           (uncurry2)
import Data.Array                  (length)

view :: TransactionId -> State -> Html Action
view tId state =
    div []
        [
          div
            [ className "page-header" ]
            [ h1 [] [ text "Transaction "
                    , small [] [text "info about transaction " <> show tId]
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
            [ className "container" ]
            [ div
                [ className "row" ]
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
                ]
            , div
                [ className "row" ]
                [ div
                    [ className "panel panel-default" ]
                    [ div
                        [ className "panel-heading" ]
                        [ text "Transaction input feed" ]
                    , table
                        [ className "table table-striped table-hover" ]
                        [ thead [] [ tr []
                            [ th [] [ text "Transaction" ]
                            , th [] [ text "Sent from" ]
                            , th [] [ text "Total sent" ]
                            , th [] [ text "Sent to" ]
                            , th [] [ text "Total received" ]
                            ]]
                        , tbody [] $ map transactionRow state.transactions
                        ]
                    ]
                ]
            ]
        ]
  where
    coinRow pid _ (Coin {getColor:Color color, getCoin:coin}) =
        tr []
           [ td [] [ text $ show pid ]
           , td [] [ text $ show color.getC ]
           , td [] [ text $ show coin]
           ]
--    txInputRow h i (Coin c) _ =
--        tr []
--           [ td [] [ text $ show h ]
--           , td [] [ text $ show i ]
--           , td [] [ text $ show c.getColor ]
--           , td [] [ text $ show c.getCoin ]
--           ]
--    txOutputRow adr (Coin c) =
--        tr []
--           [ td [] [ text $ addressToString adr ]
--           , td [] [ text $ show c.getColor ]
--           , td [] [ text $ show c.getCoin ]
--           ]
    transactionRow (TransactionSummarySerializable t) =
        tr []
           [ td [] [ link (txUrl t.txId) [] [ text $ show t.txId ] ]
           , td [] [ text $ show $ length t.txInputs ]
           , td [] [ text $ show t.txInputsTotal ]
           , td [] [ text $ show $ length t.txOutputs ]
           , td [] [ text $ show t.txOutputsTotal ]
           ]
    clickSearch = Nop
