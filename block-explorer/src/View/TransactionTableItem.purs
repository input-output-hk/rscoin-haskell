module App.View.TransactionTableItem where

import Prelude                        (($), map, show, (<<<), const, (<>), id,
                                       join, not, flip, otherwise, map, (=<<))

import App.Types                       (Action (..), State, Coin(..), Color(Color),
                                       getBalance, colorToString, addressToString,
                                       getCoins, coinToColor, WithMetadata (..),
                                       Transaction (..), TransactionExtension (..),
                                       searchQueryAddress, isTransactionExtensionOutcome,
                                       nominalDiffTimeToDateTime, TransactionExtended,
                                       SearchQuery, Address)
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

transactionTableItem :: Boolean -> Maybe Address -> TransactionExtended -> forall a. Array (Html a)
transactionTableItem colors mAddr (WithMetadata {wmValue: tran@(Transaction t), wmMetadata: tranE@(TransactionExtension te)}) =
    let addressLink mAddr' =
            div
                [ className "text-center addressLink" ]
                [ case mAddr' of
                    Just addr | mAddr `gEq` mAddr' -> text $ addressToString addr
                               | otherwise ->
                                    link (R.addressUrl addr)
                                        [ id_ "link"]
                                        [ text $ addressToString addr ]
                    Nothing -> text "Emission"
                ]
        moneyFlow tx =
            case flip isTransactionExtensionOutcome tx <$> mAddr of
                Just true -> "outcome"
                _ -> "income"
    in
        [ div
            [ className "row transaction-header no-margin" ]
            [ div
                [ className "col-xs-8 no-padding"
                , id_ "transaction-hash" ]
                [ link (R.txUrl te.teId)
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
                    [ className $ moneyFlow tranE <> "-button pull-right" ]
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
  where
    colorTableItem coin@(Coin c) =
        tr
            []
            [ visible td colors $ td
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
