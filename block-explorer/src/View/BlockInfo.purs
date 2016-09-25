module App.View.BlockInfo where

import Prelude                        (($), map, show, (<<<), const, (<>), id,
                                       join, not, flip, otherwise, map, (=<<))

import App.Types                       (Action (..), State, Coin(..), Color(Color),
                                       getBalance, colorToString, addressToString,
                                       getCoins, coinToColor, WithMetadata (..),
                                       Transaction (..), TransactionExtension (..),
                                       searchQueryAddress, isTransactionIncome,
                                       nominalDiffTimeToDateTime, TransactionExtended,
                                       HBlockExtension (..), init)
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
import Pux.Html.Events                (onChange, onClick)
import Pux.Router                     (link)
import Pux.CSS                        (style, backgroundColor, padding, px,
                                       color, white, backgroundImage, url)

import Data.Tuple                     (fst, snd)
import Data.Array                     (length, null)
import Data.Array.Partial             (tail)
import Data.Maybe                     (Maybe (..), fromMaybe)
import Data.Functor                   ((<$>))
import Data.Generic                   (gEq)
import Data.DateTime                  (diff)
import Data.Time.Duration             (Milliseconds)
import Serokell.Data.DateTime         (prettyDate, prettyDuration)

import Partial.Unsafe                 (unsafePartial)

view :: State -> Html Action
view state =
    div
        []
        [ div
            [ className "row light-grey-background" ]
            [ div
                [ id_ "section-title"
                ]
                [ ttextUpper state.language _.summary ]
            , div
                [ className "row"
                , id_ "info-table-margins" ]
                [ div
                    [ className "col-xs-12" ]
                    [ table
                        [ className "table table-striped fix-table-padding striped-dark" ]
                        [ thead
                            []
                            [ tr
                                []
                                [ th [] [ ttext' _.height ]
                                , th [] [ ttext' _.age ]
                                , th [] [ ttext' _.transactions ]
                                , th [] [ ttext' _.totalSent ]
                                ]
                            ]
                        , tbody
                            [ id_ "info-table" ]
                            $ map blockTableItem state.blocks
                        ]

                    ]
                , div
                    [ className "font-light text-center" ]
                    [ button
                        [ id_ "expand-button"
                        , onClick $ const ExpandBlockchain
                        ]
                        [ ttext' _.expand ]
                    ]
                ]
            ]
        , div
            [ className "row" ]
            [ div
                [ id_ "section-title"
                ]
                [ ttextUpper state.language _.transactionsFeed ]
            , div
                [ className "row"
                , id_ "info-table-margins" ]
                [ div
                    [ className "col-xs-12" ]
                    [ table
                        [ className "table table-striped fix-table-padding" ]
                        [ tbody
                            [ id_ "info-table" ]
                            $ map transactionsFeedItem state.transactions
                        ]

                    ]
                , div
                    [ className "font-light text-center" ]
                    [ button
                        [ id_ "expand-button"
                        , onClick $ const ExpandTransactionsGlobal
                        ]
                        [ ttext' _.expand ]
                    ]
                ]
            ]
        ]
  where
    ttext' = ttext state.language
    blockTableItem block@(HBlockExtension hbe) =
        tr
            []
            [ td
                []
                [ text $ show hbe.hbeHeight ]
            , td
                []
                [ text $ fromMaybe "Date error" $ (prettyDuration :: Milliseconds -> String) <<< diff state.now <$> nominalDiffTimeToDateTime hbe.hbeTimestamp ]
            , td
                []
                [ text $ show hbe.hbeTxNumber ]
            , td
                []
                [ img
                    [ id_ "ada-symbol"
                    , src adaSymbolDarkPath
                    ]
                    []
                , text $ show hbe.hbeTotalSent
                ]
            ]
    transactionsFeedItem (WithMetadata {wmMetadata: TransactionExtension te}) =
        tr
            []
            [ td
                []
                [ link (R.txUrl te.teId)
                    [ id_ "link" ]
                    [ text $ show te.teId ]
                ]
            , td
                []
                [ text $ fromMaybe "Date error" $ (prettyDuration :: Milliseconds -> String) <<< diff state.now <$> nominalDiffTimeToDateTime te.teTimestamp ]
            , td
                []
                [ img
                    [ id_ "ada-symbol"
                    , src adaSymbolDarkPath
                    ]
                    []
                , text $ show $ getBalance te.teOutputsSum
                ]
            ]
