module App.ViewNew.Transaction where

import Prelude                        (($), map, show)

import App.RSCoin                     (Coin(Coin), Color(Color),
                                       TransactionSummarySerializable(TransactionSummarySerializable),
                                       addressToString)
import App.Types                      (Action, State)
import App.Routes                     (txUrl, addressUrl)

import Pux.Html                       (Html, div, tbody, text, th, tr,
                                       thead, table, small, h3, td)
import Pux.Html.Attributes            (className, id_)
import Pux.Router                     (link)

import Data.Tuple.Nested              (uncurry2, uncurry4)
import Data.Array                     (length)
import Data.Maybe                     (Maybe (..))

view :: TransactionSummarySerializable -> State -> Html Action
view (TransactionSummarySerializable tx) state =
    div []
        []
--          div
--            [ className "page-header" ]
--            [ h3 [] [ text "Transaction "
--                    , small [] [ text $ show tx.txId ]
--                    ]
--            ]
--        , div
--            [ className "container" ]
--            [ div
--                [ className "row" ]
--                [ div
--                    [ className "col-xs-6" ]
--                    [ div
--                        [ className "panel panel-default" ]
--                        [ div
--                            [ className "panel-heading" ]
--                            [ text "Inputs" ]
--                        , table
--                            [ className "table table-striped table-hover" ]
--                            [ thead [] [ tr []
--                                [ th [] [ text "Address from" ]
--                                , th [] [ text "Coin color" ]
--                                , th [] [ text "Coin amount" ]
--                                ]]
--                            , tbody [] $ map (uncurry4 txInputRow) tx.txInputs
--                            ]
--                        ]
--                    ]
--                , div
--                    [ className "col-xs-6" ]
--                    [ div
--                        [ className "panel panel-default" ]
--                        [ div
--                            [ className "panel-heading" ]
--                            [ text "Outputs" ]
--                        , table
--                            [ className "table table-striped table-hover" ]
--                            [ thead [] [ tr []
--                                [ th [] [ text "Address to" ]
--                                , th [] [ text "Coin color" ]
--                                , th [] [ text "Coin amount" ]
--                                ]]
--                            , tbody [] $ map (uncurry2 txOutputRow) tx.txOutputs
--                            ]
--                        ]
--                    ]
--                ]
--            , div
--                [ className "row" ]
--                [ div
--                    [ className "col-xs-6" ]
--                    [ coinMapView tx.txInputsSum "Inputs balance" ]
--                , div
--                    [ className "col-xs-6" ]
--                    [ coinMapView tx.txOutputsSum "Outputs balance" ]
--                ]
--            ]
--        ]
--  where
--    coinRow _ (Coin {getColor:Color color, getCoin:coin}) =
--        tr []
--           [ td [] [ text $ show color.getC ]
--           , td [] [ text $ show coin]
--           ]
--    txInputRow h i (Coin {getColor:Color color, getCoin:coin}) mAddr =
--        tr []
--           [ td [] [ testEmission ]
--           , td [] [ text $ show color.getC ]
--           , td [] [ text $ show coin ]
--           ]
--      where
--        testEmission =
--            case mAddr of
--                Just addr ->  link (addressUrl addr) [] [ text $ addressToString addr ]
--                Nothing -> text "Emission"
--    txOutputRow addr (Coin {getColor:Color color, getCoin:coin}) =
--        tr []
--           [ td [] [ link (addressUrl addr) [] [ text $ addressToString addr ] ]
--           , td [] [ text $ show color.getC ]
--           , td [] [ text $ show coin]
--           ]
--    transactionRow (TransactionSummarySerializable t) =
--        tr []
--           [ td [] [ link (txUrl t.txId) [] [ text $ show t.txId ] ]
--           , td [] [ text $ show $ length t.txInputs ]
--           , td [] [ text $ show t.txInputsTotal ]
--           , td [] [ text $ show $ length t.txOutputs ]
--           , td [] [ text $ show t.txOutputsTotal ]
--           ]
--    coinMapView cs name =
--        div
--            [ className "panel panel-default" ]
--            [ div
--                [ className "panel-heading" ]
--                [ text name ]
--            , table
--                [ className "table table-striped table-hover" ]
--                [ thead [] [ tr []
--                    [ th [] [ text "Coin color" ]
--                    , th [] [ text "Coin amount" ]
--                    ]]
--                , tbody [] $ map (uncurry2 coinRow) cs
--                ]
--            ]
