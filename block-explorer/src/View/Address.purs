module App.View.Address where

import Prelude                     (($), map, show)

import App.Types                    (Action, State, Coin(Coin), Color(Color),
                                    TransactionSummarySerializable(TransactionSummarySerializable),
                                    queryToString)
import App.Routes                  (txUrl) as R

import Pux.Html (Html, tbody, text, th, tr, thead, table, div, small, h3, td)
import Pux.Html.Attributes         (className)
import Pux.Router                  (link)

import Data.Tuple.Nested           (uncurry2)
import Data.Array                  (length)
import Data.Maybe                  (fromMaybe)

view :: State -> Html Action
view state =
    div []
        [
          div
            [ className "page-header" ]
            [ h3 [] [ text "Address "
                    , small [] [ text queryInfo ]
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
    transactionRow (TransactionSummarySerializable t) =
        tr []
           [ td [] [ link (R.txUrl t.txId) [] [ text $ show t.txId ] ]
           , td [] [ text $ show $ length t.txInputs ]
           , td [] [ text $ show t.txInputsTotal ]
           , td [] [ text $ show $ length t.txOutputs ]
           , td [] [ text $ show t.txOutputsTotal ]
           ]
    queryInfo = fromMaybe "" $ map queryToString state.queryInfo
