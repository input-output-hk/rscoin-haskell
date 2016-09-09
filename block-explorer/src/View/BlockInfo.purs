module App.View.BlockInfo where

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

view :: State -> Html Action
view state =
    div
        []
        [ div
            [ className "row light-grey-background" ]
            [
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
                        [ className "table table-striped" ]
                        [ tbody
                            [ id_ "info-table" ]
                            [
                            ]
                        ]

                    ]
                ]
            ]
        ]
  where
    ttext' = ttext state.language
--    colorTableItem coin@(Coin c) =
--        tr
--            []
--            [ visible td state.colors $ td
--                []
--                [ text <<< colorToString $ coinToColor coin ]
--            , td
--                [ className "money-amount" ]
--                [ img
--                    [ id_ "ada-symbol"
--                    , src adaSymbolDarkPath
--                    ]
--                    []
--                , text <<< show $ c.getCoin
--                ]
--            ]
