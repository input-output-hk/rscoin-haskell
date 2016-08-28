module App.Types
       ( module RSCoin
       , module Color
       , Action (..)
       , SearchQuery (..)
       , init
       , State
       , queryToString
       ) where

import Prelude                     (show, class Eq)

import App.Routes                  (Route, notFoundRoute)
import App.Connection              (Connection, Action) as C
import App.RSCoin                  as RSCoin
import App.RSCoin                  (Coin, Address,
                                    TransactionSummarySerializable(TransactionSummarySerializable),
                                    addressToString)
import Data.Color                  as Color

import Data.Maybe                  (Maybe (..))
import Data.Tuple                  (Tuple)
import Data.Generic                (gEq)


data Action
    = PageView Route
    | SocketAction C.Action
    | SearchQueryChange String
    | SearchButton
    | DismissError
    | Nop

data SearchQuery
    = SQAddress Address
    | SQTransaction TransactionSummarySerializable

queryToString :: SearchQuery -> String
queryToString (SQAddress addr) = addressToString addr
queryToString (SQTransaction (TransactionSummarySerializable tId)) = show tId.txId

instance eqSearchQeuery :: Eq SearchQuery where
    eq (SQAddress addr1) (SQAddress addr2) = gEq addr1 addr2
    -- TODO: use `eq tId.txId tId2.txId` here to optimize
    eq (SQTransaction tId1) (SQTransaction tId2) = gEq tId1 tId2
    eq _ _ = false

type State =
    { route            :: Route
    , socket           :: Maybe C.Connection
    , socketReady      :: Boolean
    , pendingActions   :: Array Action
    , queryInfo        :: Maybe SearchQuery
    , isAuthenticated  :: Boolean
    , searchQuery      :: String
    , balance          :: Array (Tuple Int Coin)
    , transactions     :: Array TransactionSummarySerializable
    , periodId         :: Int
    , error            :: Maybe String
    }

init :: State
init =
    { route:            notFoundRoute
    , socket:           Nothing
    , socketReady:      false
    , pendingActions:   []
    , queryInfo:        Nothing
    , isAuthenticated:  false
    , searchQuery:      ""
    , balance:          []
    , transactions:     []
    , periodId:         0
    , error:            Nothing
    }
