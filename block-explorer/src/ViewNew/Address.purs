module App.ViewNew.Address where

import Prelude                        (($), map, show)

import App.Types                       (Action, State, Coin(Coin), Color(Color),
                                       TransactionSummarySerializable(TransactionSummarySerializable),
                                       queryToString)
import App.Routes                     (txUrl) as R
import App.CSS                        (darkRed, opacity, logoPath, lightGrey,
                                       headerBitmapPath, noBorder, adaSymbolPath)

import Pux.Html                       (Html, tbody, text, th, tr, thead,
                                       table, div, small, h3, td, img)
import Pux.Html.Attributes            (aria, data_, type_,
                                       placeholder, value, src, alt)
import Pux.Router                     (link)
import Pux.CSS                        (style, backgroundColor, padding, px,
                                       color, white, backgroundImage, url)

import Data.Tuple.Nested              (uncurry2)
import Data.Array                     (length)
import Data.Maybe                     (fromMaybe)

import Serokell.Pux.Html              (classNames, className)
import Serokell.Pux.Themes.Bootstrap3 as B

view :: State -> Html Action
view state =
    div []
        [ div
            [ style do
                color darkRed
            ]
            [ h3 [] [ text "ADDRESS"
                    ]
            ]
        , div
            [ className B.row ]
            [ div
                [ className B.colXs8 ]
                [ table
                    [ classNames [B.table] ]
                    [ tbody
                        []
                        [ tr
                            []
                            [ td [ style noBorder ] [ text "Address" ]
                            , td [ style noBorder ] [ text "oqpwieoqweipqie" ]
                            ]
                        , tr
                            [ style $ backgroundColor lightGrey ]
                            [ td [ style noBorder ] [ text "Transactions" ]
                            , td [ style noBorder ] [ text "127" ]
                            ]
                        , tr
                            []
                            [ td [ style noBorder ] [ text "Final balance" ]
                            , td
                                [ style noBorder ]
                                [ img
                                    [ alt "Brand"
                                    , src adaSymbolPath
                                    ]
                                    []
                                , text "213.12"
                                ]
                            ]
                        ]
                    ]

                ]
            , div
                [ className B.colXs4 ]
                []
            ]
        ]
