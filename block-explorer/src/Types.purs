module App.Types where

import App.Routes                  (Route(NotFound))
import App.Connection              (Connection, Action (..)) as C
import App.RSCoin                  (emptyAddress, Address, Coin (..),
                                    TransactionSummarySerializable (..),
                                    Color (..), IntroductoryMsg)

import Data.Maybe                  (Maybe (..))
import Data.Tuple                  (Tuple (..))


data Action
    = PageView Route
    | SocketAction C.Action
    | AddressChange Address
    | Search
    | DismissError
    | Nop

type State =
    { route        :: Route
    , socket       :: Maybe C.Connection
    , addressInfo  :: Maybe IntroductoryMsg
    , address      :: Address
    , balance      :: Array (Tuple Color Coin)
    , transactions :: Array TransactionSummarySerializable
    , periodId     :: Int
    , error        :: Maybe String
    }

init :: State
init =
    { route:        NotFound
    , socket:       Nothing
    , addressInfo:  Nothing
    , address:      emptyAddress
    , balance:      []
    , transactions: []
    , periodId:     0
    , error:        Nothing
    }
