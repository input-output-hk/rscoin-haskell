module App.View.Transaction where

import Prelude                     (($), map, (<<<), const, pure, bind, show,
                                    (==), (<>))

import App.RSCoin                  (emptyAddress, Address, newAddress,
                                    addressToString, IntroductoryMsg (..),
                                    AddressInfoMsg (..), Coin (..),
                                    TransactionSummarySerializable (..),
                                    OutcomingMsg (..), Color (..),
                                    TransactionId)
import App.Types                   (Action (..), State (..))
import App.Connection              (Action (..)) as C
import App.Routes                  (txUrl, addressUrl)

import Pux                         (EffModel, noEffects)
import Pux.Html                    (Html, div, h3, text, input, button,
                                    small, h5, span, table, tr, th, td,
                                    thead, tbody)
import Pux.Html.Attributes         (type_, value, rel, href, className)
import Pux.Html.Events             (onChange, onClick, onKeyDown)
import Pux.Router                  (link)

import Data.Tuple.Nested           (uncurry2, uncurry4)
import Data.Array                  (length)
import Data.Maybe                  (Maybe (..))

view :: TransactionSummarySerializable -> State -> Html Action
view (TransactionSummarySerializable tx) state =
    div []
        [
          div
            [ className "page-header" ]
            [ h3 [] [ text "Transaction "
                    , small [] [ text $ show tx.txId ]
                    ]
            ]
        , div
            [ className "container" ]
            [ div
                [ className "row" ]
                [ div
                    [ className "col-xs-6" ]
                    [ div
                        [ className "panel panel-default" ]
                        [ div
                            [ className "panel-heading" ]
                            [ text "Inputs" ]
                        , table
                            [ className "table table-striped table-hover" ]
                            [ thead [] [ tr []
                                [ th [] [ text "Address from" ]
                                , th [] [ text "Coin color" ]
                                , th [] [ text "Coin amount" ]
                                ]]
                            , tbody [] $ map (uncurry4 txInputRow) tx.txInputs
                            ]
                        ]
                    ]
                , div
                    [ className "col-xs-6" ]
                    [ div
                        [ className "panel panel-default" ]
                        [ div
                            [ className "panel-heading" ]
                            [ text "Outputs" ]
                        , table
                            [ className "table table-striped table-hover" ]
                            [ thead [] [ tr []
                                [ th [] [ text "Address to" ]
                                , th [] [ text "Coin color" ]
                                , th [] [ text "Coin amount" ]
                                ]]
                            , tbody [] $ map (uncurry2 txOutputRow) tx.txOutputs
                            ]
                        ]
                    ]
                ]
            , div
                [ className "row" ]
                [ div
                    [ className "col-xs-6" ]
                    [ coinMapView tx.txInputsSum "Inputs balance" ]
                , div
                    [ className "col-xs-6" ]
                    [ coinMapView tx.txOutputsSum "Outputs balance" ]
                ]
            ]
        ]
  where
    coinRow _ (Coin {getColor:Color color, getCoin:coin}) =
        tr []
           [ td [] [ text $ show color.getC ]
           , td [] [ text $ show coin]
           ]
    txInputRow h i (Coin {getColor:Color color, getCoin:coin}) mAddr =
        tr []
           [ td [] [ testEmission ]
           , td [] [ text $ show color.getC ]
           , td [] [ text $ show coin ]
           ]
      where
        testEmission =
            case mAddr of
                Just addr ->  link (addressUrl addr) [] [ text $ addressToString addr ]
                Nothing -> text "Emission"
    txOutputRow addr (Coin {getColor:Color color, getCoin:coin}) =
        tr []
           [ td [] [ link (addressUrl addr) [] [ text $ addressToString addr ] ]
           , td [] [ text $ show color.getC ]
           , td [] [ text $ show coin]
           ]
    transactionRow (TransactionSummarySerializable t) =
        tr []
           [ td [] [ link (txUrl t.txId) [] [ text $ show t.txId ] ]
           , td [] [ text $ show $ length t.txInputs ]
           , td [] [ text $ show t.txInputsTotal ]
           , td [] [ text $ show $ length t.txOutputs ]
           , td [] [ text $ show t.txOutputsTotal ]
           ]
    coinMapView cs name =
        div
            [ className "panel panel-default" ]
            [ div
                [ className "panel-heading" ]
                [ text name ]
            , table
                [ className "table table-striped table-hover" ]
                [ thead [] [ tr []
                    [ th [] [ text "Coin color" ]
                    , th [] [ text "Coin amount" ]
                    ]]
                , tbody [] $ map (uncurry2 coinRow) cs
                ]
            ]
