module App.ViewNew.Address where

import Prelude                        (($), map, show, (<<<), const, (<>))

import App.Types                       (Action (..), State, Coin(Coin), Color(Color),
                                       TransactionSummarySerializable(TransactionSummarySerializable),
                                       queryToString)
import App.Routes                     (txUrl, getQueryParams) as R
import App.CSS                        (darkRed, opacity, logoPath, lightGrey,
                                       headerBitmapPath, noBorder, adaSymbolPath,
                                       adaSymbolDarkPath, transactionArrowGreenPath,
                                       transactionArrowRedPath)

import Pux.Html                       (Html, tbody, text, th, tr, thead, a, span,
                                       table, div, small, h3, td, img, ul, li,
                                       input, label, button)
import Pux.Html.Attributes            (aria, data_, type_, className, id_,
                                       placeholder, value, src, alt, role, href,
                                       autoComplete, htmlFor, rowSpan, checked)
import Pux.Html.Events                (onChange)
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
                    [ className "col-xs-8 no-padding-only-right" ]
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
                                        [ text "1NPj2Y8yswHLuw8Yr1FDdobKAW6WVkUZy9" ]
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
                                        [ id_ "ada-symbol"
                                        , src adaSymbolDarkPath
                                        ]
                                        []
                                    , text "213,12"
                                    , div
                                        [ className "pull-right" ]
                                        [ label
                                            [ className "switch" ]
                                            [ input
                                                [ type_ "checkbox"
                                                , onChange $ const ColorToggle
                                                , checked state.colors
                                                ]
                                                []
                                            , div
                                                [ className "slider round" ]
                                                []
                                            ]
                                        ]
                                    , div
                                        [ className "pull-right" ]
                                        [ text "Color balance" ]
                                    ]
                                ]
                            ]
                        ]

                    ]
                , div
                    [ className "col-xs-4 no-padding-only-left" ]
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
                                    [ className "table no-margin" ]
                                    [ tbody
                                        [ id_ "color-table" ]
                                        [ tr
                                            []
                                            [ td [] [ text "Red" ]
                                            , td
                                                []
                                                [ img
                                                    [ id_ "ada-symbol"
                                                    , src adaSymbolDarkPath
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
                                                    [ id_ "ada-symbol"
                                                    , src adaSymbolDarkPath
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
                                                    [ id_ "ada-symbol"
                                                    , src adaSymbolDarkPath
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
                                                    [ id_ "ada-symbol"
                                                    , src adaSymbolDarkPath
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
                                                    [ id_ "ada-symbol"
                                                    , src adaSymbolDarkPath
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
                                    [ className "table no-margin" ]
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
                [ className "row no-margin" ]
                [ div
                    [ id_ "section-title"
                    ]
                    [ text "TRANSACTIONS" ]
                ]
            , div
                [ id_ "info-table-margins" ]
                [ div
                    [ className "row transaction-header no-margin" ]
                    [ div
                        [ className "col-xs-8 no-padding"
                        , id_ "transaction-hash" ]
                        [ a
                            [ id_ "link" ]
                            [ text "6a3b06baa79d555d1bf7caac383deb4dbcb37e957fb270c0147cbd75b9a8d3a6" ]
                        ]
                    , div
                        [ className "col-xs-4 no-padding-only-left" ]
                        [ div
                            [ className "pull-left"
                            , id_ "transaction-date" ]
                            [ text "2016-07-08 11:56:48" ]
                        , button
                            [ className "income-button pull-right" ]
                            [ img
                                [ id_ "ada-symbol"
                                , src adaSymbolPath
                                ]
                                []
                            , text "213,128,124,000"
                            ]
                        ]
                    ]
                , div
                    [ className "row transaction-body no-margin" ]
                    [ div
                        [ className "col-xs-8 no-padding-only-right" ]
                        [ table
                            [ className "table"
                            , id_ "transaction-addresses-table" ]
                            [ tbody
                                []
                                [ tr
                                    []
                                    [ td
                                        []
                                        [ a
                                            [ id_ "link"]
                                            [ text "13dXD6C4KQVqnZGFTmuZzrVjWkH9pgc9Ng" ]
                                        ]
                                    , td
                                        [ rowSpan 3
                                        , id_ "spanned-cell" ]
                                        [ img
                                            [ id_ "transaction-arrow"
                                            , src transactionArrowGreenPath
                                            ]
                                            []
                                        ]
                                    , td
                                        [ rowSpan 3
                                        , id_ "spanned-cell" ]
                                        [ text "1NPj2Y8yswHLuw8Yr1FDdobKAW6WVkUZy9" ]
                                    ]
                                , tr
                                    []
                                    [ td
                                        []
                                        [ a
                                            [ id_ "link"]
                                            [ text "zrVjWkH9pgc9Ng13dXD6C4KQVqnZGFTmuZ" ]
                                        ]
                                    ]
                                , tr
                                    []
                                    [ td
                                        []
                                        [ a
                                            [ id_ "link"]
                                            [ text "gc9Ng13dXD6C4KQVqnZGFTmuZzrVjWkH9p" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , div
                        [ className "col-xs-4 no-padding-only-left" ]
                        [ table
                            [ className "table"
                            , id_ "transaction-addresses-table" ]
                            [ tbody
                                []
                                [ tr
                                    []
                                    [ td [] [ text "Red" ]
                                    , td
                                        [ id_ "money-amount" ]
                                        [ img
                                            [ id_ "ada-symbol"
                                            , src adaSymbolDarkPath
                                            ]
                                            []
                                        , text "71,2929"
                                        ]
                                    ]
                                , tr
                                    []
                                    [ td [] [ text "Blue" ]
                                    , td
                                        [ id_ "money-amount" ]
                                        [ img
                                            [ id_ "ada-symbol"
                                            , src adaSymbolDarkPath
                                            ]
                                            []
                                        , text "71,2929"
                                        ]
                                    ]
                                , tr
                                    []
                                    [ td [] [ text "Blue" ]
                                    , td
                                        [ id_ "money-amount" ]
                                        [ img
                                            [ id_ "ada-symbol"
                                            , src adaSymbolDarkPath
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
                    [ className "row transaction-header no-margin" ]
                    [ div
                        [ className "col-xs-8 no-padding"
                        , id_ "transaction-hash" ]
                        [ a
                            [ id_ "link" ]
                            [ text "6a3b06baa79d555d1bf7caac383deb4dbcb37e957fb270c0147cbd75b9a8d3a6" ]
                        ]
                    , div
                        [ className "col-xs-4 no-padding-only-left" ]
                        [ div
                            [ className "pull-left"
                            , id_ "transaction-date" ]
                            [ text "2016-07-08 11:56:48" ]
                        , button
                            [ className "outcome-button pull-right" ]
                            [ img
                                [ id_ "ada-symbol"
                                , src adaSymbolPath
                                ]
                                []
                            , text "-124,000"
                            ]
                        ]
                    ]
                , div
                    [ className "row transaction-body no-margin" ]
                    [ div
                        [ className "col-xs-8 no-padding-only-right" ]
                        [ table
                            [ className "table"
                            , id_ "transaction-addresses-table" ]
                            [ tbody
                                []
                                [ tr
                                    []
                                    [ td
                                        [ rowSpan 2
                                        , id_ "spanned-cell" ]
                                        [ text "1NPj2Y8yswHLuw8Yr1FDdobKAW6WVkUZy9" ]
                                    , td
                                        [ rowSpan 2
                                        , id_ "spanned-cell" ]
                                        [ img
                                            [ id_ "transaction-arrow"
                                            , src transactionArrowRedPath
                                            ]
                                            []
                                        ]
                                    , td
                                        []
                                        [ a
                                            [ id_ "link"]
                                            [ text "13dXD6C4KQVqnZGFTmuZzrVjWkH9pgc9Ng" ]
                                        ]
                                    ]
                                , tr
                                    []
                                    [ td
                                        []
                                        [ a
                                            [ id_ "link"]
                                            [ text "zrVjWkH9pgc9Ng13dXD6C4KQVqnZGFTmuZ" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , div
                        [ className "col-xs-4 no-padding-only-left" ]
                        [ table
                            [ className "table"
                            , id_ "transaction-addresses-table" ]
                            [ tbody
                                []
                                [ tr
                                    []
                                    [ td [] [ text "Red" ]
                                    , td
                                        [ id_ "money-amount" ]
                                        [ img
                                            [ id_ "ada-symbol"
                                            , src adaSymbolDarkPath
                                            ]
                                            []
                                        , text "-71,2929"
                                        ]
                                    ]
                                , tr
                                    []
                                    [ td [] [ text "Blue" ]
                                    , td
                                        [ id_ "money-amount" ]
                                        [ img
                                            [ id_ "ada-symbol"
                                            , src adaSymbolDarkPath
                                            ]
                                            []
                                        , text "-71,2929"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div
                    [ className "center-block"
                    , id_ "pagination" ]
                    [ span
                        [ className "glyphicon glyphicon-triangle-left"
                        , id_ "navigation-arrow" ]
                        []
                    ,  input
                        [ type_ "search"
                        , id_ "pagination-search"
                        , placeholder "1200"
                        ]
                        []
                    , span
                        [ className "navagation-text-span"
                        , id_ "disabled-text" ]
                        [ text "of" ]
                    , span
                        [ className "navagation-text-span" ]
                        [ text "9090" ]
                    , span
                        [ className "glyphicon glyphicon-triangle-right"
                        , id_ "navigation-arrow" ]
                        []
                    ]
                --, button
                --    [ id_ "expand-button" ]
                --    [ text "expand-button" ]
                ]
            ]
        ]
