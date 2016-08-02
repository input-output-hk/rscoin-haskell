module App.Layout where

import Prelude                     (($), map, (<<<), const, pure, bind,
                                    (==), flip, (<>), (/=), otherwise)

import App.Routes                  (Route (..), addressUrl, homeUrl, txUrl, match) as R
import App.Connection              (Action (..), WEBSOCKET,
                                    introMessage, send) as C
import App.Types                   (Address (..), IntroductoryMsg (..),
                                    AddressInfoMsg (..),
                                    TransactionSummarySerializable (..),
                                    OutcomingMsg (..),
                                    Action (..), State, SearchQuery (..),
                                    PublicKey (..), ServerError (..), Hash (..))
import App.View.Address            (view) as Address
import App.View.NotFound           (view) as NotFound
import App.View.Transaction        (view) as Transaction

import Data.Maybe                  (Maybe(Nothing, Just), maybe, fromJust,
                                    isNothing, isJust)

import Data.Tuple                  (Tuple (..), snd)
import Data.Either                 (fromRight)
import Data.Generic                (gShow)
import Data.Array                  (filter, head)
import Debug.Trace                 (traceAny)

import Pux                         (EffModel, noEffects, onlyEffects)
import Pux.Html                    (Html, div, text, strong, span, button,
                                    input, a, li, ul, nav, link)

import Pux.Router                  (navigateTo, link) as R
import Pux.Html.Attributes         (className, aria, data_, type_, role,
                                    placeholder, value, href, rel)

import Pux.Html.Events             (onChange, onClick, onKeyDown)

import Control.Apply               ((*>))
import Control.Alternative         ((<|>))
import Control.Applicative         (when, unless)

import DOM                         (DOM)
import Control.Monad.Eff.Console   (CONSOLE)
import Control.Monad.Eff.Class     (liftEff)

import Partial.Unsafe              (unsafePartial)

txNum :: Int
txNum = 15

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ws :: C.WEBSOCKET, dom :: DOM)
update (PageView route@(R.Address addr)) state =
    { state: state { route = route }
    , effects:
        [ onNewQueryDo do
            if state.isAuthenticated
                then C.send socket' $ AIChangeAddress addr
                else C.introMessage socket' $ IMAddressInfo addr
            pure Nop
        -- FIXME: if socket isn't opened open some error page
        ]
    }
  where
    socket' = unsafePartial $ fromJust state.socket
    onNewQueryDo action | state.queryInfo == Just (SQAddress addr) = pure Nop -- ignore
                        | otherwise = action
update (PageView route@(R.Transaction tId)) state =
    { state: state { route = route, queryInfo = map SQTransaction getTransaction }
    , effects:
        [ onNewQueryDo do
            when (isNothing getTransaction) $
                C.introMessage socket' $ IMTransactionInfo tId
            pure Nop
        ]
    }
  where
    socket' = unsafePartial $ fromJust state.socket
    getTransaction =
        queryGetTx state.queryInfo
        <|>
        head (filter (\(TransactionSummarySerializable t) -> t.txId == tId) state.transactions)
    queryGetTx (Just (SQTransaction tx@(TransactionSummarySerializable t)))
        | t.txId == tId = Just tx
    queryGetTx _ = Nothing
    onNewQueryDo action | isJust getTransaction = pure Nop -- ignore
                        | otherwise = action
update (PageView route) state = noEffects $ state { route = route }
update (SocketAction (C.ReceivedData msg)) state = traceAny (gShow msg) $
    \_ -> case unsafePartial $ fromRight msg of
        OMBalance pId arr ->
            { state: state { balance = arr, periodId = pId }
            , effects:
                [ do
                    C.send socket' <<< AIGetTransactions $ Tuple 0 txNum
                    pure Nop
                ]
            }
        OMTransactions _ arr ->
            noEffects $ state { transactions = map snd arr }
        OMTransaction tx@(TransactionSummarySerializable t) ->
            { state: state { queryInfo = Just $ SQTransaction tx }
            , effects:
                [ do
                    let expectedUrl = R.txUrl t.txId
                    unless (state.route == R.match expectedUrl) $
                        liftEff $ R.navigateTo expectedUrl
                    pure Nop
                ]
            }
        OMSessionEstablished addr ->
            { state: state { queryInfo = Just $ SQAddress addr, isAuthenticated = true }
            , effects:
                [ do
                    C.send socket' AIGetTxNumber
                    C.send socket' AIGetBalance
                    let expectedUrl = R.addressUrl addr
                    unless (state.route == R.match expectedUrl) $
                        liftEff $ R.navigateTo expectedUrl
                    pure Nop
                ]
            }
        OMError (ParseError e) ->
            noEffects $ state { error = Just $ "ParseError: " <> e }
        OMError (NotFound e) ->
            noEffects $ state { error = Just $ "NotFound: " <> e }
        _ -> noEffects state
  where
    socket' = unsafePartial $ fromJust state.socket
update (SocketAction _) state = noEffects state
update (SearchQueryChange sq) state = noEffects $ state { searchQuery = sq }
update SearchButton state =
    onlyEffects state $
        [ do
            if state.isAuthenticated
                then C.send socket' $ AIChangeInfo addr tId
                else C.introMessage socket' $ IMInfo addr tId
            pure Nop
        ]
  where
    socket' = unsafePartial $ fromJust state.socket
    addr = Address { getAddress: PublicKey state.searchQuery }
    tId = Hash state.searchQuery
update DismissError state = noEffects $ state { error = Nothing }
update Nop state = noEffects state

-- TODO: make safe version of bootstrap like
-- https://github.com/slamdata/purescript-halogen-bootstrap/blob/master/src/Halogen/Themes/Bootstrap3.purs
view :: State -> Html Action
view state =
    div
        []
        [ link
            [ rel "stylesheet"
            , type_ "text/css"
            , href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            ]
            []
            -- TODO: uncoment these scripts if you will be using more advanced
            -- things from bootstrap that require js
--        , script
--            [ src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js" ]
--            []
--        , script
--            [ src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" ]
--            []
        , nav
            [ className "navbar navbar-default" ]
            [ div
                [ className "container-fluid" ]
                [ div
                    [ className "navbar-header" ]
                    [ R.link R.homeUrl
                        [ className "navbar-brand"
                        ]
                        [ text "RS | COIN" ]
                    ]
                , ul
                    [ className "nav navbar-nav navbar-right" ]
                    [ li
                        [ className "dropdown" ]
                        [ a
                            [ className "dropwdown-toggle"
                            , data_ "target" "#"
                            , data_ "toggle" "dropdown"
                            , aria "haspopup" "true"
                            , aria "expanded" "false"
                            ]
                            [ text "English"
                            , span
                                [ className "caret" ]
                                []
                            ]
                        , ul
                            [ className "dropdown-menu" ]
                            [ li
                                []
                                [ a
                                    [ data_ "target" "#" ]
                                    [ text "English" ]
                                ]
                            ]
                        ]
                    ]
                , div
                    [ className "col-xs-6 navbar-form navbar-right" ]
                    [ div
                        [ className "input-group" ]
                        [ input
                            [ type_ "text"
                            , value state.searchQuery
                            , onChange $ SearchQueryChange <<< _.value <<< _.target
                            , onKeyDown $ \e -> if e.keyCode == 13 then SearchButton else Nop
                            , className "form-control"
                            , placeholder "Address / Transaction"
                            ] []
                        , span
                            [ className "input-group-btn" ]
                            [ button
                                [ onClick $ const SearchButton
                                , className "btn btn-danger"
                                ] [ text "Search" ]
                            ]
                        ]
                    ]
                ]
            ]
        , case state.error of
            Just e ->
                div
                    [ className "alert alert-danger alert-dismissible"
                    , role "alert"
                    ]
                    [ button
                        [ type_ "button"
                        , className "close"
                        , data_ "dismiss" "alert"
                        , aria "label" "Close"
                        , onClick $ const DismissError
                        ]
                        [ span
                            [ aria "hidden" "true" ]
                            [ text "×" ]
                        ]
                    , strong
                        []
                        [ text "Error! " ]
                    , text e
                    ]
            Nothing -> div [] []
        , div
            [ className "container-fluid" ]
            [ case state.route of
                R.Home -> Address.view state
                R.Address _ -> Address.view state
                R.Transaction tId ->
		            let
                        queryGetTx (Just (SQTransaction tx)) = Just tx
                        queryGetTx _ = Nothing
					in  maybe (NotFound.view state) (flip Transaction.view state) $ queryGetTx state.queryInfo
                R.NotFound -> NotFound.view state
            ]
        ]
