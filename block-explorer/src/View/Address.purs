module App.View.Address where

import Prelude                        (($), map, show)

import App.Types                       (Action, State, Coin(Coin), Color(Color),
                                       TransactionSummarySerializable(TransactionSummarySerializable),
                                       queryToString)
import App.Routes                     (txUrl) as R

import Pux.Html (Html, tbody, text,    th, tr, thead, table, div, small, h3, td)
import Pux.Router                     (link)

import Data.Tuple.Nested              (uncurry2)
import Data.Array                     (length)
import Data.Maybe                     (fromMaybe)

import Serokell.Pux.Html              (classNames, className)
import Serokell.Pux.Themes.Bootstrap3 as B

view :: State -> Html Action
view state =
    div []
        []
--           div
--             [ className B.pageHeader ]
--             [ h3 [] [ text "Address "
--                     , small [] [ text queryInfo ]
--                     ]
--             ]
--         , div
--             [ className B.container ]
--             [ div
--                 [ className B.row ]
--                 [ div
--                     [ classNames [B.panel, B.panelDefault] ]
--                     [ div
--                         [ className B.panelHeading ]
--                         [ text "Balance" ]
--                     , table
--                         [ classNames [B.table, B.tableStriped, B.tableHover] ]
--                         [ thead [] [ tr []
--                             [ th [] [ text "Height" ]
--                             , th [] [ text "Coin color" ]
--                             , th [] [ text "Coin amount" ]
--                             ]]
--                         --, tbody [] $ map (uncurry2 $ coinRow state.periodId) state.balance.cmCoinsMap
--                         ]
--                     ]
--                 ]
--             , div
--                 [ className B.row ]
--                 [ div
--                     [ classNames [B.panel, B.panelDefault] ]
--                     [ div
--                         [ className B.panelHeading ]
--                         [ text "Transaction input feed" ]
--                     , table
--                         [ classNames [B.table, B.tableStriped, B.tableHover] ]
--                         [ thead [] [ tr []
--                             [ th [] [ text "Transaction" ]
--                             , th [] [ text "Sent from" ]
--                             , th [] [ text "Total sent" ]
--                             , th [] [ text "Sent to" ]
--                             , th [] [ text "Total received" ]
--                             ]]
--                         , tbody [] $ map transactionRow state.transactions
--                         ]
--                     ]
--                 ]
--             ]
--         ]
--   where
--     coinRow pid _ (Coin {getColor:Color color, getCoin:coin}) =
--         tr []
--            [ td [] [ text $ show pid ]
--            , td [] [ text $ show color.getC ]
--            , td [] [ text $ show coin]
--            ]
--     transactionRow (TransactionSummarySerializable t) =
--         tr []
--            [ td [] [ link (R.txUrl t.txId) [] [ text $ show t.txId ] ]
--            , td [] [ text $ show $ length t.txInputs ]
--            --, td [] [ text $ show t.txInputsTotal ]
--            , td [] [ text $ show $ length t.txOutputs ]
--            --, td [] [ text $ show t.txOutputsTotal ]
--            ]
--     queryInfo = fromMaybe "" $ map queryToString state.queryInfo
