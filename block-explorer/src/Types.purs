module App.Types where

import App.Routes                  (Route(NotFound))
import App.Connection              (Connection, Action (..)) as C
import App.RSCoin                  (emptyAddress, Address, Coin (..),
                                    TransactionSummarySerializable (..),
                                    Color (..))

import Data.Maybe                  (Maybe (..))
import Data.Tuple                  (Tuple (..))


data Action
    = PageView Route
    | SocketAction C.Action
    | AddressChange Address
    | Nop

type State =
    { route        :: Route
    , socket       :: Maybe C.Connection
    , address      :: Address
    , balance      :: Array (Tuple Color Coin)
    , transactions :: Array TransactionSummarySerializable
    , periodId     :: Int
    }

init :: State
init =
    { route:        NotFound
    , socket:       Nothing
    , address:      emptyAddress
    , balance:      []
    , transactions: []
    , periodId:     0
    }
