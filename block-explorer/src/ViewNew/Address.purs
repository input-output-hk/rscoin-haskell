module App.ViewNew.Address where

import Prelude                        (($), map, show)

import App.Types                       (Action, State, Coin(Coin), Color(Color),
                                       TransactionSummarySerializable(TransactionSummarySerializable),
                                       queryToString)
import App.Routes                     (txUrl) as R
import App.CSS                        (darkRed, opacity, logoPath, lightGrey,
                                       headerBitmapPath, noBorder, adaSymbolPath)

import Pux.Html                       (Html, tbody, text, th, tr, thead, a, span,
                                       table, div, small, h3, td, img, ul, li,
                                       input, label, button)
import Pux.Html.Attributes            (aria, data_, type_, className, id_,
                                       placeholder, value, src, alt, role, href,
                                       autoComplete, htmlFor)
import Pux.Router                     (link)
import Pux.CSS                        (style, backgroundColor, padding, px,
                                       color, white, backgroundImage, url)

import Data.Tuple.Nested              (uncurry2)
import Data.Array                     (length)
import Data.Maybe                     (fromMaybe)

view :: State -> Html Action
view state =
    div
        []
        [ div
            [ className "row" ]
            [ div
                [ id_ "section-title"
                ]
                [ text "ADDRESS" ]
            , div
                [ className "row"
                , id_ "info-table-margins" ]
                [ div
                    [ className "col-xs-8"
                    , id_ "no-padding-only-right" ]
                    [ table
                        [ className "table" ]
                        [ tbody
                            [ id_ "info-table" ]
                            [ tr
                                []
                                [ td [] [ text "Address" ]
                                , td
                                    []
                                    [ a
                                        [ id_ "address-link" ]
                                        [ text "oqpwieoqweipqie" ]
                                    ]
                                ]
                            , tr
                                [ className "light-grey-background" ]
                                [ td [] [ text "Transactions" ]
                                , td [] [ text "127" ]
                                ]
                            , tr
                                []
                                [ td [] [ text "Final balance" ]
                                , td
                                    []
                                    [ img
                                        [ id_ "ada-symbol-dark"
                                        , src adaSymbolPath
                                        ]
                                        []
                                    , text "213,12"
                                    , div
                                        [ className "pull-right" ]
                                        [ text "Color balance"
                                        , span
                                            []
                                            [ input
                                                [ type_ "checkbox"
                                                , id_ "color-toggle"
                                                ]
                                                []
                                            , label
                                                [ htmlFor "color-toggle" ]
                                                []
                                                -- @sasha: we can use http://www.bootstraptoggle.com/
                                                -- or some other implementation if you prefer it. Please just let me know and I will replace them
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]

                    ]
                , div
                    [ className "col-xs-4"
                    , id_ "no-padding-only-left" ]
                    [ -- @sasha: this is from http://getbootstrap.com/javascript/#markup
                      -- and from http://getbootstrap.com/components/#nav-tabs
                      ul
                        [ className "nav nav-pills" -- try experimenting with either nav-tabs or nav-pills classes .. I think nav-pills would require less overrides to match our style. There is laso nav-justified if we want to use it
                        -- we could even use fade effect if it looks good to you http://getbootstrap.com/javascript/#fade-effect
                        , role "tablist"
                        , id_ "tabs"
                        ]
                        [ li
                            [ role "presentation"
                            , className "active"
                            ]
                            [ a
                                [ href "#color-balance"
                                , id_ "color-balance-tab"
                                , aria "controls" "color-balance"
                                , role "tab"
                                , data_ "toggle" "tab"
                                ]
                                [ text "Color balance" ]
                            ]
                        , li
                            [ role "presentation"
                            , className ""
                            ]
                            [ a
                                [ href "#qr-code"
                                , id_ "qr-code-tab"
                                , aria "controls" "qr-code"
                                , role "tab"
                                , data_ "toggle" "tab"
                                ]
                                [ text "QR Code" ]
                            ]
                        ]
                    , div
                        [ className "tab-content" ]
                        [ div
                            [ role "tabpanel"
                            , className "tab-pane active"
                            , id_ "color-balance"
                            , aria "labelledby" "color-balance-tab"
                            ]
                            [ div
                                [ id_ "color-table-overflow" ]
                                [ table
                                    [ className "table"
                                    , id_ "no-margin" ]
                                    [ tbody
                                        [ id_ "color-table" ]
                                        [ tr
                                            []
                                            [ td [] [ text "Red" ]
                                            , td
                                                []
                                                [ img
                                                    [ id_ "ada-symbol-dark"
                                                    , src adaSymbolPath
                                                    ]
                                                    []
                                                , text "71,2929"
                                                ]
                                            ]
                                        , tr
                                            []
                                            [ td [] [ text "Blue" ]
                                            , td
                                                []
                                                [ img
                                                    [ id_ "ada-symbol-dark"
                                                    , src adaSymbolPath
                                                    ]
                                                    []
                                                , text "71,2929"
                                                ]
                                            ]
                                        , tr
                                            []
                                            [ td [] [ text "Blue" ]
                                            , td
                                                []
                                                [ img
                                                    [ id_ "ada-symbol-dark"
                                                    , src adaSymbolPath
                                                    ]
                                                    []
                                                , text "71,2929"
                                                ]
                                            ]
                                        , tr
                                            []
                                            [ td [] [ text "Blue" ]
                                            , td
                                                []
                                                [ img
                                                    [ id_ "ada-symbol-dark"
                                                    , src adaSymbolPath
                                                    ]
                                                    []
                                                , text "71,2929"
                                                ]
                                            ]
                                        , tr
                                            []
                                            [ td [] [ text "Blue" ]
                                            , td
                                                []
                                                [ img
                                                    [ id_ "ada-symbol-dark"
                                                    , src adaSymbolPath
                                                    ]
                                                    []
                                                , text "71,2929"
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , div
                            [ role "tabpanel"
                            , className "tab-pane"
                            , id_ "qr-code"
                            , aria "labelledby" "qr-code-tab"
                            ]
                            [ div
                                [ id_ "qr-code" ]
                                [ table
                                    [ className "table"
                                    , id_ "no-margin" ]
                                    [ tbody
                                        []
                                        [ tr
                                            []
                                            [ td
                                                [ id_ "qr-code-cell" ]
                                                [ img
                                                    [ src "http://www.appcoda.com/wp-content/uploads/2013/12/qrcode.jpg"
                                                    , id_ "qr-code-img"
                                                    ]
                                                    []
                                            ]
                                            , td
                                                [ id_ "qr-code-text-cell" ]
                                                [ text "Scan this QR Code to copy address to clipboard"
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div
            [ className "row light-grey-background" ]
            [ div
                [ className "row" ]
                [ div
                    [ id_ "section-title"
                    ]
                    [ text "TRANSACTIONS" ]
                ]
            , div
                [ className "row"
                , id_ "info-table-margins"
                ]
                [ div
                    [ className "col-xs-8" ]
                    [ text "dkskjsdalkjdlaksjdlaksjdalksjd" ]
                , div
                    [ className "col-xs-4" ]
                    [ div
                        [ className "pull-left" ]
                        [ text "2016-07-08 11:56:48" ]
                    , button
                        [ className "btn btn-success pull-right" ]
                        [ img
                            [ id_ "ada-symbol-dark"
                            , src adaSymbolPath
                            ]
                            []
                        , text "213,12"
                        ]
                    ]
                ]
            , div
                [ className "row" ]
                [ div
                    [ className "col-xs-8" ]
                    [ table
                        [ className "table"
                        , id_ "no-margin" ]
                        [ tbody
                            [ id_ "color-table" ]
                            [ tr
                                []
                                [ td [] [ text "ksajdlksajdlksajdlskajd" ]
                                , td
                                    []
                                    [ -- have to import image here
                                    ]
                                , td [] [ text "ksajdlksajdlksajdlskajd" ]
                                ]
                            , tr
                                []
                                [ td [] [ text "ksajdlksajdlksajdlskajd" ]
                                , td
                                    []
                                    [ -- have to import image here
                                    ]
                                , td [] [ text "ksajdlksajdlksajdlskajd" ]
                                ]
                            , tr
                                []
                                [ td [] [ text "ksajdlksajdlksajdlskajd" ]
                                , td
                                    []
                                    [ -- have to import image here
                                    ]
                                , td [] [ text "ksajdlksajdlksajdlskajd" ]
                                ]
                            ]
                        ]
                    ]
                , div
                    [ className "col-xs-4" ]
                    [ table
                        [ className "table"
                        , id_ "no-margin" ]
                        [ tbody
                            [ id_ "color-table" ]
                            [ tr
                                []
                                [ td [] [ text "Red" ]
                                , td
                                    []
                                    [ img
                                        [ id_ "ada-symbol-dark"
                                        , src adaSymbolPath
                                        ]
                                        []
                                    , text "71,2929"
                                    ]
                                ]
                            , tr
                                []
                                [ td [] [ text "Blue" ]
                                , td
                                    []
                                    [ img
                                        [ id_ "ada-symbol-dark"
                                        , src adaSymbolPath
                                        ]
                                        []
                                    , text "71,2929"
                                    ]
                                ]
                            , tr
                                []
                                [ td [] [ text "Blue" ]
                                , td
                                    []
                                    [ img
                                        [ id_ "ada-symbol-dark"
                                        , src adaSymbolPath
                                        ]
                                        []
                                    , text "71,2929"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div
                [ className "row"
                , id_ "info-table-margins"
                ]
                [ div
                    [ className "col-xs-8" ]
                    [ text "dkskjsdalkjdlaksjdlaksjdalksjd" ]
                , div
                    [ className "col-xs-4" ]
                    [ div
                        [ className "pull-left" ]
                        [ text "2016-07-08 11:56:48" ]
                    , button
                        [ className "btn btn-success pull-right" ]
                        [ img
                            [ id_ "ada-symbol-dark"
                            , src adaSymbolPath
                            ]
                            []
                        , text "213,12"
                        ]
                    ]
                ]
            , div
                [ className "row" ]
                [ div
                    [ className "col-xs-8" ]
                    [ table
                        [ className "table"
                        , id_ "no-margin" ]
                        [ tbody
                            [ id_ "color-table" ]
                            [ tr
                                []
                                [ td [] [ text "ksajdlksajdlksajdlskajd" ]
                                , td
                                    []
                                    [ -- have to import image here
                                    ]
                                , td [] [ text "ksajdlksajdlksajdlskajd" ]
                                ]
                            , tr
                                []
                                [ td [] [ text "ksajdlksajdlksajdlskajd" ]
                                , td
                                    []
                                    [ -- have to import image here
                                    ]
                                , td [] [ text "ksajdlksajdlksajdlskajd" ]
                                ]
                            , tr
                                []
                                [ td [] [ text "ksajdlksajdlksajdlskajd" ]
                                , td
                                    []
                                    [ -- have to import image here
                                    ]
                                , td [] [ text "ksajdlksajdlksajdlskajd" ]
                                ]
                            ]
                        ]
                    ]
                , div
                    [ className "col-xs-4" ]
                    [ table
                        [ className "table"
                        , id_ "no-margin" ]
                        [ tbody
                            [ id_ "color-table" ]
                            [ tr
                                []
                                [ td [] [ text "Red" ]
                                , td
                                    []
                                    [ img
                                        [ id_ "ada-symbol-dark"
                                        , src adaSymbolPath
                                        ]
                                        []
                                    , text "71,2929"
                                    ]
                                ]
                            , tr
                                []
                                [ td [] [ text "Blue" ]
                                , td
                                    []
                                    [ img
                                        [ id_ "ada-symbol-dark"
                                        , src adaSymbolPath
                                        ]
                                        []
                                    , text "71,2929"
                                    ]
                                ]
                            , tr
                                []
                                [ td [] [ text "Blue" ]
                                , td
                                    []
                                    [ img
                                        [ id_ "ada-symbol-dark"
                                        , src adaSymbolPath
                                        ]
                                        []
                                    , text "71,2929"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
