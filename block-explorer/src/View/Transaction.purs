module App.View.Transaction where

import Prelude                        (($), map, show, (<<<), const, (<>), id,
                                       join, not, flip, otherwise, map, (=<<))

import App.Types                       (Action (..), State, Coin(..), Color(Color),
                                       getBalance, colorToString, addressToString,
                                       getCoins, coinToColor, WithMetadata (..),
                                       Transaction (..), TransactionExtension (..),
                                       searchQueryAddress, isTransactionIncome,
                                       nominalDiffTimeToDateTime, TransactionExtended)
import App.View.TransactionTableItem  (transactionTableItem)
import App.Routes                     (txUrl, addressUrl, toUrl, getQueryParams) as R
import App.CSS                        (darkRed, opacity, logoPath, lightGrey,
                                       headerBitmapPath, noBorder, adaSymbolPath,
                                       adaSymbolDarkPath, transactionArrowGreenPath,
                                       transactionArrowRedPath)
import App.Common.Html                (ttext, ttextUpper, visible)

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

import Data.Tuple                     (fst, snd)
import Data.Array                     (length, null)
import Data.Array.Partial             (tail)
import Data.Maybe                     (Maybe (..), fromMaybe)
import Data.Functor                   ((<$>))
import Data.Generic                   (gEq)
import Serokell.Data.DateTime         (prettyDate)

import Partial.Unsafe                 (unsafePartial)

view :: TransactionExtended -> State -> Html Action
view teFull@(WithMetadata {wmValue: tran@(Transaction t), wmMetadata: tranE@(TransactionExtension te)}) state =
    div
        []
        [ div
            [ className "row light-grey-background" ]
            [ div
                [ className "row no-margin" ]
                [ div
                    [ id_ "section-title"
                    ]
                    [ ttextUpper state.language _.transaction ]
                ]
            , div
                [ id_ "info-table-margins" ]
                $ transactionTableItem state.colors state.queryInfo teFull
            ]
        , div
            [ className "row" ]
            [ div
                [ id_ "section-title"
                ]
                [ ttextUpper state.language _.summary ]
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
                                [ td [] [ ttext' _.receivedTime ]
                                , td [] [ text $ fromMaybe "Date error" $ prettyDate <$> nominalDiffTimeToDateTime te.teTimestamp ]
                                ]
                            , tr
                                [ className "light-grey-background" ]
                                [ td [] [ ttext' _.includedInBlocks ]
                                , td [] [ text $ show $ te.tePeriodId ]
                                ]
                            , tr
                                []
                                [ td [] [ ttext' _.totalInput ]
                                , td
                                    []
                                    [ img
                                        [ id_ "ada-symbol"
                                        , src adaSymbolDarkPath
                                        ]
                                        []
                                    , text $ show <<< getBalance $ te.teInputsSum
                                    ]
                                ]
                            , tr
                                [ className "light-grey-background" ]
                                [ td [] [ ttext' _.totalOutput ]
                                , td
                                    []
                                    [ img
                                        [ id_ "ada-symbol"
                                        , src adaSymbolDarkPath
                                        ]
                                        []
                                    , text $ show <<< getBalance $ te.teOutputsSum
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
                                        [ className "pull-right font-light" ]
                                        [ ttext' _.colorBalance ]
                                    ]
                                ]
                            ]
                        ]

                    ]
                , div
                    [ className "col-xs-4 no-padding-only-left" ]
                    [ ul
                        [ className "nav nav-pills"
                        , role "tablist"
                        , id_ "tabs"
                        ]
                        [ visible li state.colors $ li
                            [ role "presentation"
                            , className $ colorsActive id
                            ]
                            [ a
                                [ href "#color-balance"
                                , id_ "color-balance-tab"
                                , aria "controls" "color-balance"
                                , role "tab"
                                , data_ "toggle" "tab"
                                ]
                                [ ttext' _.colorBalance ]
                            ]
                        ]
                    , div
                        [ className "tab-content" ]
                        [ visible div state.colors $ div
                            [ role "tabpanel"
                            , className $ "tab-pane " <> colorsActive id
                            , id_ "color-balance"
                            , aria "labelledby" "color-balance-tab"
                            ]
                            [ div
                                [ id_ "color-table-overflow" ]
                                [ table
                                    [ className "table no-margin" ]
                                    [ tbody
                                        [ id_ "color-table" ]
                                        $ map (colorTableItem <<< snd) t.txOutputs
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
  where
    ttext' = ttext state.language
    colorTableItem coin@(Coin c) =
        tr
            []
            [ visible td state.colors $ td
                []
                [ text <<< colorToString $ coinToColor coin ]
            , td
                [ className "money-amount" ]
                [ img
                    [ id_ "ada-symbol"
                    , src adaSymbolDarkPath
                    ]
                    []
                , text <<< show $ c.getCoin
                ]
            ]
    colorsActive f = if f state.colors then "active" else ""
    searchAddress = join $ map searchQueryAddress state.queryInfo
