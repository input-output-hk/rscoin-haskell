module App.Types
       ( module RSCoin
       , Action (..)
       , SearchQuery (..)
       , init
       , State
       , queryToString
       ) where

import Prelude                     (show, class Eq, eq)

import App.Routes                  (Route(NotFound))
import App.Connection              (Connection, Action (..)) as C
import App.RSCoin                  as RSCoin
import App.RSCoin                  (emptyAddress, Address, Coin (..),
                                    TransactionSummarySerializable (..),
                                    Color (..), IntroductoryMsg, TransactionId,
                                    addressToString)

import Data.Maybe                  (Maybe (..))
import Data.Tuple                  (Tuple (..))
import Data.Generic                (class Generic, gEq)


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
    , addressConnected :: Boolean
    , searchQuery      :: String
    , balance          :: Array (Tuple Color Coin)
    , transactions     :: Array TransactionSummarySerializable
    , periodId         :: Int
    , error            :: Maybe String
    }

init :: State
init =
    { route:            NotFound
    , socket:           Nothing
    , socketReady:      false
    , pendingActions:   []
    , queryInfo:        Nothing
    , addressConnected: false
    , searchQuery:      ""
    , balance:          []
    , transactions:     []
    , periodId:         0
    , error:            Nothing
    }
