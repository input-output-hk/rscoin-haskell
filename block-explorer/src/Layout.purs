module App.Layout where

import Prelude                        (($), map, (<<<), pure, bind, not, (*), max, min,
                                       (==), flip, (<>), (/=), otherwise, (+), (-),
                                       Unit, (=<<), show, (&&))
import Prelude                        (div) as P

import App.Routes                     (Path (..), addressUrl, txUrl,
                                       match) as R
import App.Connection                 (Action (..), WEBSOCKET,
                                       send, sendControl, Connection) as C
import App.Types                      (Address (..), ControlMsg (..),
                                       AddressInfoMsg (..),
                                       IncomingMsg (..),
                                       TransactionExtended,
                                       OutcomingMsg (..),
                                       Action (..), State, SearchQuery (..),
                                       PublicKey (..), ServerError (..), Hash (..),
                                       getTransactionId, HBlockInfoMsg (..), HBlockExtension (..), getTransactionTimestamp)
import App.CSS                        (veryLightGrey, styleSheet)
import App.View.Address               (view) as Address
import App.View.NotFound              (view) as NotFound
import App.View.BlockInfo             (view) as BlockInfo
import App.View.Transaction           (view) as Transaction
import App.View.Header                (view) as Header
import App.View.Alert                 (view) as Alert
import App.View.Footer                (view) as Footer

import Data.Maybe                     (Maybe(Nothing, Just), maybe,
                                       isNothing, isJust, fromMaybe)

import Serokell.Data.Maybe            (unsafeFromJust)
import Serokell.Data.Char             (isDigit, isSpace)

import Data.Tuple                     (Tuple (..), snd)
import Data.Either                    (fromRight)
import Data.Generic                   (gShow, gEq)
import Data.Array                     (filter, head, reverse, length, singleton, range,
                                       take, nubBy, sortBy)
import Data.Functor                   ((<$>))
import Data.Traversable               (traverse)
import Data.Ord                       (compare)
import Debug.Trace                    (traceAny, traceAnyM)
import Data.Int                       (fromString)
import Data.String                    (toCharArray, fromCharArray)
import Data.String                    (take) as S
import Data.Enum                      (pred, succ)

import Pux                            (EffModel, noEffects, onlyEffects)
import Pux.Html                       (Html, div, style, text)
import Pux.Html.Attributes            (type_, id_, className)

import Pux.Router                     (navigateTo) as R
import Pux.CSS                        (style, backgroundColor) as CSS
import CSS.Render                     (renderedSheet, render)

import Control.Apply                  ((*>))
import Control.Alternative            ((<|>))
import Control.Applicative            (when, unless)

import DOM                            (DOM)
import Control.Monad.Eff.Console      (CONSOLE, log)
import Control.Monad.Eff.Now          (nowDateTime, NOW)
import Control.Monad.Eff.Class        (liftEff)
import Control.Monad.Aff              (Aff)
import Control.Comonad                (extract)

import Partial.Unsafe                 (unsafePartial)

removeWhiteSpace :: String -> String
removeWhiteSpace = fromCharArray <<< filter (not <<< isSpace) <<< toCharArray

removeNonDigit :: String -> String
removeNonDigit = fromCharArray <<< filter isDigit <<< toCharArray

txNum :: Int
txNum = 5

blocksNum :: Int
blocksNum = 5

txGlobalNum :: Int
txGlobalNum = 5

paginationNum :: Int
paginationNum = 10

-- NOTE: this should be less then or equal to RSCoin.Explorer.Web.Sockets.App.transactionsLimit
txLimit :: Int
txLimit = 80

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ws :: C.WEBSOCKET, dom :: DOM, now :: NOW)
update (PageView route@R.Home) state =
    { state: state { route = route }
    , effects:
        [ onNewQueryDo do
            C.send socket' $ IMControl CMUnsubscribeAddress
            C.send socket' $ IMControl CMGetBlockchainHeight
            pure Nop
        ]
    }
  where
    socket' = unsafeFromJust state.socket
    -- TODO: update other `onNewQuery` with this function!
    onNewQueryDo action | state.route == route = pure Nop -- ignore
                        | otherwise = action
update (PageView route@(R.Address addr)) state =
    { state: state { route = route, blocks = [], paginationPage = "", paginationExpand = true }
    , effects:
        [ onNewQueryDo do
            C.send socket' $ IMControl CMUnsubscribeNewBlocks
            C.send socket' $ IMControl $ CMSetAddress addr
            pure Nop
        ]
    }
  where
    socket' = unsafeFromJust state.socket
    onNewQueryDo action | state.queryInfo == Just (SQAddress addr) = pure Nop -- ignore
                        | otherwise = action
update (PageView route@(R.Transaction tId)) state =
    { state: state { route = route, queryInfo = map SQTransaction getTransaction, transactions = [], blocks = [], paginationPage = "", paginationExpand = true }
    , effects:
        [ onNewQueryDo do
            when (isNothing getTransaction) $ do
                C.send socket' $ IMControl CMUnsubscribeAddress
                C.send socket' $ IMControl CMUnsubscribeNewBlocks
                C.send socket' $ IMControl $ CMGetTransaction tId
            pure Nop
        ]
    }
  where
    socket' = unsafeFromJust state.socket
    getTransaction =
        queryGetTx state.queryInfo
        <|>
        head (filter ((==) tId <<< getTransactionId) state.transactions)
    queryGetTx (Just (SQTransaction tx))
        | getTransactionId tx == tId = Just tx
    queryGetTx _ = Nothing
    onNewQueryDo action | isJust getTransaction = pure Nop -- ignore
                        | otherwise = action
update (PageView route) state = noEffects $ state { route = route }
update (SocketAction (C.ReceivedData msg)) state = traceAny (gShow msg) $
    \_ -> case unsafePartial $ fromRight msg of
        OMBalance addr pId coinsMap ->
            { state: state { balance = Just coinsMap, periodId = pId, queryInfo = Just (SQAddress addr) }
            , effects:
                [ do
                    transactionPage socket' paginationPage
                    let expectedUrl = R.addressUrl addr
                    unless (state.route == R.match expectedUrl) $
                        liftEff $ R.navigateTo expectedUrl
                    pure Nop
                ]
            }
        OMAddrTransactions addr _ arr ->
            let transactionTimeComp t1 t2 =
                    getTransactionTimestamp t1 `compare` getTransactionTimestamp t2
            in noEffects $ state
                   { transactions =
                       -- TODO: this is really inefficient, like 2*(N^2) or worse.
                 --      sortBy transactionTimeComp $
                 --      unsafePartial $ take (length state.transactions) $
                 --      nubBy gEq $
                       take (pagination txGlobalNum $ length state.transactions) $ map snd arr
                   , queryInfo = Just (SQAddress addr)
                   }
        OMTransaction _ tx ->
            { state: state { queryInfo = Just $ SQTransaction tx }
            , effects:
                [ do
                    let expectedUrl = R.txUrl $ getTransactionId tx
                    unless (state.route == R.match expectedUrl) $
                        liftEff $ R.navigateTo expectedUrl
                    pure Nop
                ]
            }
        OMTxNumber addr _ (Tuple txNum _) ->
            noEffects $ state { txNumber = Just txNum, queryInfo = Just (SQAddress addr) }
        OMBlocksOverview blocks ->
            -- NOTE: nubBy is needed because live update and expand buttone could be triggered at the same time
            noEffects $ state { blocks = take (pagination blocksNum $ length state.blocks) $ reverse $ map snd blocks }
        OMTransactionsGlobal _ txs ->
            noEffects $ state { transactions = state.transactions <> map snd txs }
        OMBlockchainHeight pId ->
            { state: state { periodId = pId, transactions = [], blocks = [], paginationPage = "", paginationExpand = true }
            , effects:
                [ do
                    blockchainPage socket' pId paginationPage
                    C.send socket' $ IMControl $ CMGetTransactionsGlobal $ Tuple 0 txGlobalNum
                    C.send socket' $ IMControl $ CMSetHBlock pId
                    pure Nop
                ]
            }
        OMNewBlock pId ->
            { state: state { periodId = pId }
            , effects:
                [ do
                    C.send socket' $ IMControl $ CMSetHBlock pId
                    C.send socket' $ IMHBlockInfo HIGetMetadata
                    blockchainPage socket' pId paginationPage
                    pure Nop
                ]
            }
        OMBlockMetadata _ block@(HBlockExtension {hbeTxNumber}) ->
            let transactionSlices =
                    map (\i -> Tuple (i*txLimit) $ (i+1)*txLimit) $
                    range 0 (hbeTxNumber `P.div` txLimit)
            in { state: state
               , effects:
                   [ do
                        traverse
                            (C.send socket' <<< IMHBlockInfo <<< HIGetTransactions)
                            transactionSlices
                        pure Nop
                   ]
               }
        OMBlockTransactions _ txs ->
            noEffects $ state
                { transactions =
                    -- NOTE: nubBy is needed because live update and expand buttone could be triggered at the same time
                    unsafePartial $ take (pagination txGlobalNum $ length state.transactions) $
                    nubBy gEq $
                    map snd txs <> state.transactions
                }
        OMError (ParseError e) ->
            noEffects $ state { error = Just $ "ParseError: " <> e.peTypeName <> " : " <> e.peError }
        OMError (NotFound e) ->
            noEffects $ state { error = Just $ "NotFound: " <> e }
        OMError (LogicError e) ->
            noEffects $ state { error = Just $ "LogicError: " <> e }
        _ -> noEffects state
  where
    pagination max' len =
        if state.paginationExpand
            then max max' len
            else paginationNum
    socket' = unsafeFromJust state.socket
    paginationPage = min 9999 $ max 0 $ fromMaybe 0 $ fromString state.paginationPage
update (SocketAction _) state = noEffects state
update (SearchQueryChange sq) state = noEffects $ state { searchQuery = sq }
update SearchButton state =
    onlyEffects state $
        [ do
            C.send socket' $ IMControl $ CMSmart state.searchQuery
            pure Nop
        ]
  where
    socket' = unsafeFromJust state.socket
update DismissError state = noEffects $ state { error = Nothing }
update TransactionsToggle state =
    noEffects $ state { showTransactions = not state.showTransactions }
update ColorToggle state =
    noEffects $ state { colors = state.isAdmin && not state.colors }
update (LanguageSet l) state =
    noEffects $ state { language = l }
update UpdateClock state = onlyEffects state $
    [ do
         SetClock <<< extract <$> liftEff nowDateTime
    ]
update (SetClock date) state = noEffects $ state { now = date }
update ExpandTransactions state =
    { state: state { paginationExpand = false }
    , effects:
        [ do
            transactionPage socket' paginationPage
            pure Nop
        ]
    }
  where
    socket' = unsafeFromJust state.socket
    paginationPage = min 9999 $ max 0 $ fromMaybe 0 $ fromString state.paginationPage
update ExpandTransactionsGlobal state = onlyEffects state $
    [ do
        let txLen = length state.transactions
        C.send socket' $ IMControl $ CMGetTransactionsGlobal $
            Tuple txLen $ txLen + txGlobalNum
        pure Nop
    ]
  where
    socket' = unsafeFromJust state.socket
update ExpandBlockchain state =
    { state: state { paginationExpand = false }
    , effects:
        [ do
            blockchainPage socket' state.periodId 0
            pure Nop
        ]
    }
  where
    socket' = unsafeFromJust state.socket
update (PaginationUpdate page) state = noEffects $ state { paginationPage = S.take 4 $ removeNonDigit page }
update PaginationLeftBlocks state =
    { state: state { paginationPage = show paginationPage }
    , effects:
        [ do
            blockchainPage socket' state.periodId paginationPage
            pure Nop
        ]
    }
  where
    socket' = unsafeFromJust state.socket
    paginationPage = min 9999 $ max 0 $ fromMaybe 0 $ pred =<< fromString state.paginationPage
update PaginationRightBlocks state =
    { state: state { paginationPage = show paginationPage }
    , effects:
        [ do
            blockchainPage socket' state.periodId paginationPage
            pure Nop
        ]
    }
  where
    socket' = unsafeFromJust state.socket
    paginationPage = min 9999 $ max 0 $ fromMaybe 0 $ succ =<< fromString state.paginationPage
update PaginationLeftTransactions state =
    { state: state { paginationPage = show paginationPage }
    , effects:
        [ do
            transactionPage socket' paginationPage
            pure Nop
        ]
    }
  where
    socket' = unsafeFromJust state.socket
    paginationPage = min 9999 $ max 0 $ fromMaybe 0 $ pred =<< fromString state.paginationPage
update PaginationRightTransactions state =
    { state: state { paginationPage = show paginationPage }
    , effects:
        [ do
            transactionPage socket' paginationPage
            pure Nop
        ]
    }
  where
    socket' = unsafeFromJust state.socket
    paginationPage = min 9999 $ max 0 $ fromMaybe 0 $ succ =<< fromString state.paginationPage

update PaginationSearchBlocks state = onlyEffects state $
    [ do
        blockchainPage socket' state.periodId paginationPage
        pure Nop
    ]
  where
    socket' = unsafeFromJust state.socket
    paginationPage = min 9999 $ max 0 $ fromMaybe 0 $ fromString state.paginationPage
update PaginationSearchTransactions state = onlyEffects state $
    [ do
        transactionPage socket' paginationPage
        pure Nop
    ]
  where
    socket' = unsafeFromJust state.socket
    paginationPage = min 9999 $ max 0 $ fromMaybe 0 $ fromString state.paginationPage
update Nop state = noEffects state

-- blockchainPage :: forall eff. C.Connection -> Int -> Int -> Aff (channel :: CHANNEL | eff) Unit
blockchainPage socket' pId page = do
    let pageOffset = page * paginationNum
    C.send socket' $ IMControl $ CMGetBlocksOverview $
        Tuple (pId - paginationNum - pageOffset) $ pId + 1 - pageOffset

transactionPage socket' page = do
    let pageOffset = page * paginationNum
    C.send socket' <<< IMAddrInfo <<< AIGetTransactions $ Tuple pageOffset $ paginationNum + pageOffset + 1

-- TODO: make safe version of bootstrap like
-- https://github.com/slamdata/purescript-halogen-bootstrap/blob/master/src/Halogen/Themes/Bootstrap3.purs
view :: State -> Html Action
view state =
    div
        [ className "very-light-grey-background max-height" ]
        [-- style
         --   [ type_ "text/css" ]
         --   [ text $ unsafePartial $ fromJust $ renderedSheet $ render styleSheet ]
          Header.view state
        , Alert.view state
        , div
            [ className "container-fluid"
            , id_ "page-content"
            ]
            [ case state.route of
                R.Home -> BlockInfo.view state
                R.Address addr ->
                    let
                        queryGetAddr (Just (SQAddress addr)) = Just addr
                        queryGetAddr _ = Nothing
                    in  maybe (NotFound.view state) (flip Address.view state) $ queryGetAddr state.queryInfo
                R.Transaction tId ->
                    let
                        queryGetTx (Just (SQTransaction tx)) = Just tx
                        queryGetTx _ = Nothing
                    in  maybe (NotFound.view state) (flip Transaction.view state) $ queryGetTx state.queryInfo
                R.NotFound -> NotFound.view state
            ]
        , Footer.view state
        ]
