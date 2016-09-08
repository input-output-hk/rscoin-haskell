module App.View.Address where

import Prelude                        (($), map, show, (<<<), const, (<>), id,
                                       join, not, flip, otherwise, map, (=<<))

import App.Types                       (Action (..), State, Coin(..), Color(Color),
                                       getBalance, colorToString, addressToString,
                                       getCoins, coinToColor, WithMetadata (..),
                                       Transaction (..), TransactionExtension (..),
                                       searchQueryAddress, isTransactionIncome,
                                       nominalDiffTimeToDateTime)
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

view :: State -> Html Action
view state =
    div
        []
        [ div
            [ className "row" ]
            [ div
                [ id_ "section-title"
                ]
                [ ttextUpper state.language _.address ]
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
                                [ td [] [ ttext' _.address ]
                                , td
                                    []
                                    [ link
                                        (R.toUrl state.route)
                                        [ id_ "address-link" ]
                                        [ text $ fromMaybe "" $ addressToString <$> searchAddress ]
                                    ]
                                ]
                            , tr
                                [ className "light-grey-background" ]
                                [ td [] [ ttext' _.transactions ]
                                , td [] [ text $ show $ fromMaybe 0 state.txNumber ]
                                ]
                            , tr
                                []
                                [ td [] [ ttext' _.finalBalance ]
                                , td
                                    []
                                    [ img
                                        [ id_ "ada-symbol"
                                        , src adaSymbolDarkPath
                                        ]
                                        []
                                    , text $ fromMaybe "0" $ show <<< getBalance <$> state.balance
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
                        , li
                            [ role "presentation"
                            , className $ colorsActive not
                            ]
                            [ a
                                [ href "#qr-code"
                                , id_ "qr-code-tab"
                                , aria "controls" "qr-code"
                                , role "tab"
                                , data_ "toggle" "tab"
                                ]
                                [ ttext' _.qrCode ]
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
                                        $ fromMaybe [] $ map colorTableItem <<< getCoins <$> state.balance
                                    ]
                                ]
                            ]
                        , div
                            [ role "tabpanel"
                            , className $ "tab-pane " <> colorsActive not
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
                                                    -- FIXME: please check this api. Is it safe to use it? Maybe we shouldn't trust this third party for this functionality?
                                                    [ src $ "https://api.qrserver.com/v1/create-qr-code/?size=100x100&data=" <> fromMaybe "Error" (addressToString <$> searchAddress)
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
                    [ ttextUpper state.language _.transactions ]
                ]
            , div
                [ id_ "info-table-margins" ]
                -- NOTE: =<< == foldMap == concatMap
                $ (=<<) transactionTableItem state.transactions
                <>
                [ div
                    [ className "row transaction-body no-margin" ]
                    [ div
                        [ className "col-xs-8 no-padding-only-right" ]
                        [
                        ]
                    ]
                , div
                    [ className "center-block font-light"
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
                , div
                    [ className "font-light" ]
                    [
            --, button
            --    [ id_ "expand-button"
            --    ]
            --    [ text "expand-button" ]
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
    moneyFlow tx =
        case flip isTransactionIncome tx <$> searchAddress of
            Just true -> "income"
            _ -> "outcome"
    transactionTableItem (WithMetadata {wmValue: tran@(Transaction t), wmMetadata: TransactionExtension te}) =
        let addressLink mAddr =
                div
                    [ className "text-center addressLink" ]
                    [ case mAddr of
                        Just addr | mAddr `gEq` searchAddress ->
                                        a
                                            [ id_ "link"]
                                            [ text $ addressToString addr ]
                                  | otherwise -> text $ addressToString addr
                        Nothing -> text "Emission"
                    ]
        in
            [ div
                [ className "row transaction-header no-margin" ]
                [ div
                    [ className "col-xs-8 no-padding"
                    , id_ "transaction-hash" ]
                    [ a
                        [ id_ "link" ]
                        [ text $ show te.teId ]
                    ]
                , div
                    [ className "col-xs-4 no-padding-only-left" ]
                    [ div
                        [ className "pull-left"
                        , id_ "transaction-date" ]
                        [ text $ fromMaybe "Date error" $ prettyDate <$> nominalDiffTimeToDateTime te.teTimestamp ]
                    , button
                        [ className $ moneyFlow tran <> "-button pull-right" ]
                        [ img
                            [ id_ "ada-symbol"
                            , src adaSymbolPath
                            ]
                            []
                        , text $ show $ getBalance te.teOutputsSum
                        ]
                    ]
                ]
            , div
                [ className "row transaction-body no-margin" ]
                [ div
                    [ className "col-xs-8 no-padding-only-right" ]
                    [ table
                        [ className "table fix-tx-table"
                        , id_ "transaction-addresses-table" ]
                        [ tbody
                            []
                            [ tr
                                []
                                [ td [] $ map addressLink $
                                    if null te.teInputAddresses
                                        then [Nothing]
                                        else te.teInputAddresses
                                , td
                                    []
                                    [ img
                                        [ id_ "transaction-arrow"
                                        , src transactionArrowGreenPath
                                        ]
                                        []
                                    ]
                                , td [] $ map (addressLink <<< Just <<< fst) t.txOutputs
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
                            $ map (colorTableItem <<< snd) t.txOutputs
                        ]
                    ]
                ]
            ]
