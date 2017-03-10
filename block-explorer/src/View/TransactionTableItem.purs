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
                                       adaSymbolDarkPath, transactionArrowIncomePath,
                                       transactionArrowOutcomePath, transactionArrowNeutralPath)
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
import Data.I18N                      (Language)
import Serokell.Data.DateTime         (prettyDate)

import Partial.Unsafe                 (unsafePartial)

import App.Common.Hash (unescapeB64U)

transactionTableItem :: Language -> Boolean -> Maybe Address -> TransactionExtended -> forall a. Array (Html a)
transactionTableItem lang colors mAddr (WithMetadata {wmValue: tran@(Transaction t), wmMetadata: tranE@(TransactionExtension te)}) =
    let addressLink mAddr' =
            div
                [ className "text-center addressLink" ]
                [ case mAddr' of
                    Just addr | mAddr `gEq` mAddr' -> text $ unescapeB64U $ addressToString addr
                               | otherwise ->
                                    link (R.addressUrl addr)
                                        [ id_ "link"]
                                        [ text $ unescapeB64U $ addressToString addr ]
                    Nothing -> ttext lang _.emission
                ]
        moneyFlow tx outcome income neutral =
            case flip isTransactionExtensionOutcome tx <$> mAddr of
                Just true -> outcome
                Just false -> income
                Nothing -> neutral
    in
        [ div
            [ className "row transaction-header no-margin" ]
            [ div
                [ className "col-xs-8 no-padding"
                , id_ "transaction-hash" ]
                [ link (R.txUrl te.teId)
                    [ id_ "link" ]
                    [ text $ unescapeB64U $ show te.teId ]
                ]
            , div
                [ className "col-xs-4 no-padding-only-left" ]
                [ div
                    [ className "pull-left"
                    , id_ "transaction-date" ]
                    [ text $ fromMaybe "Date error" $ prettyDate <$> nominalDiffTimeToDateTime te.teTimestamp ]
                , button
                    [ className $ "money-button " <> moneyFlow tranE "outcome" "income" "neutral" <> "-button pull-right" ]
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
                            [ td [] $ map addressLink $ testEmission $
                                if colors
                                    then te.teInputAddresses
                                    else map (Just <<< fst) te.teSumPerInputAddr
                            , td
                                []
                                [ img
                                    [ id_ "transaction-arrow"
                                    , src $ moneyFlow tranE transactionArrowOutcomePath transactionArrowIncomePath transactionArrowNeutralPath
                                    ]
                                    []
                                ]
                            , td [] $ map addressLink $
                                if colors
                                    then map (Just <<< fst) t.txOutputs
                                    else map (Just <<< fst) te.teSumPerOutputAddr
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
                        $ if colors
                            then map (colorTableItem <<< snd) t.txOutputs
                            else map (colorAmountTableItem <<< snd) te.teSumPerOutputAddr
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
                , text <<< show $ c.coinAmount
                ]
            ]
    colorAmountTableItem coinAmount =
        tr
            []
            [ td
                [ className "money-amount" ]
                [ img
                    [ id_ "ada-symbol"
                    , src adaSymbolDarkPath
                    ]
                    []
                , text $ show coinAmount
                ]
            ]
    testEmission addresses =
        if null addresses
            then [Nothing]
            else addresses
