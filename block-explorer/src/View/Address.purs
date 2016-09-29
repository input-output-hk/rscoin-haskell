module App.View.Address where

import Prelude                        (($), map, show, (<<<), const, (<>), id,
                                       join, not, flip, otherwise, map, (=<<),
                                       (<), (==))
import Prelude                        (div) as P

import App.Types                       (Action (..), State, Coin(..), Color(Color),
                                       getBalance, colorToString, addressToString,
                                       getCoins, coinToColor, WithMetadata (..),
                                       Transaction (..), TransactionExtension (..),
                                       searchQueryAddress, isTransactionIncome,
                                       nominalDiffTimeToDateTime, Address)
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
import Pux.Html.Events                (onChange, onClick, onKeyDown)
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

view :: Address -> State -> Html Action
view addr state =
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
                        [ className "table table-striped" ]
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
                                        [ text $ addressToString addr ]
                                    ]
                                ]
                            , tr
                                []
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
                                                    [ src $ "https://api.qrserver.com/v1/create-qr-code/?size=100x100&data=" <> addressToString addr
                                                    , id_ "qr-code-img"
                                                    ]
                                                    []
                                            ]
                                            , td
                                                [ id_ "qr-code-text-cell" ]
                                                [ ttext' _._qrCodeMsg
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
                $ (=<<) (transactionTableItem state.language state.colors $ Just addr) state.transactions
                <>
                [ div
                    [ className "row transaction-body no-margin" ]
                    [ div
                        [ className "col-xs-8 no-padding-only-right" ]
                        [
                        ]
                    ]
                -- TODO: refactor! there is the same funcitonality in BlockInfo.purs
                , if state.paginationExpand
                    then
                        div
                            [ className "font-light text-center" ]
                            [ button
                                [ id_ "expand-button"
                                , onClick $ const ExpandTransactions
                                ]
                                [ ttext' _.expand ]
                            ]
                    else
                        div
                            [ className "center-block font-light"
                            , id_ "pagination" ]
                            [ span
                                [ className "glyphicon glyphicon-triangle-left"
                                , onClick $ const PaginationLeft
                                , id_ "navigation-arrow" ]
                                []
                            ,  input
                                [ type_ "search"
                                , id_ "pagination-search"
                                , value state.paginationPage

                                , onChange $ PaginationUpdate <<< _.value <<< _.target
                                , onKeyDown $ \e -> if e.keyCode == 13 then PaginationSearchTransactions else Nop
                                ]
                                []
                            , span
                                [ className "navagation-text-span"
                                , id_ "disabled-text" ]
                                [ text "of" ]
                            , span
                                [ className "navagation-text-span" ]
                                -- FIXME: don't hardcode this
                                [ text $ show $ (fromMaybe 0 $ state.txNumber) `P.div` 10 ]
                            , span
                                [ className "glyphicon glyphicon-triangle-right"
                                , onClick $ const PaginationRight
                                , id_ "navigation-arrow" ]
                                []
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
                , text <<< show $ c.coinAmount
                ]
            ]
    colorsActive f = if f state.colors then "active" else ""
