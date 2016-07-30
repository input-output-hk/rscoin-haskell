module App.View.Address where

import Prelude                     (($), map, (<<<), const, pure, bind, show,
                                    (==))

import App.RSCoin                  (emptyAddress, Address, newAddress,
                                    addressToString, IntroductoryMsg (..),
                                    AddressInfoMsg (..), Coin (..),
                                    TransactionSummarySerializable (..),
                                    OutcomingMsg (..), Color (..))
import App.Types                   (Action (..), State (..))
import App.Connection              (Action (..)) as C
import App.Routes                  (txUrl, Route (..), addressUrl) as R

import Pux                         (EffModel, noEffects)
import Pux.Html                    (Html, div, h3, text, input, button,
                                    small, h5, span, table, tr, th, td,
                                    thead, tbody)
import Pux.Html.Attributes         (type_, value, rel, href, className)
import Pux.Html.Events             (onChange, onClick, onKeyDown)
import Pux.Router                  (link)

import Data.Tuple.Nested           (uncurry2)
import Data.Array                  (length, head)
import Data.Maybe                  (fromMaybe)

view :: State -> Html Action
view state =
    div []
        [
          div
            [ className "page-header" ]
            [ h3 [] [ text "Address "
                    , small [] [ text address ]
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
    address = fromMaybe "" <<< map (\(IMAddressInfo a) -> addressToString a) $ head state.addressInfo
