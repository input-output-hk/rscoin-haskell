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
    { route          :: Route
    , socket         :: Maybe C.Connection
    , socketReady    :: Boolean
    , pendingActions :: Array Action
    -- FIXME: this is not optimal, if messages come out of order we could
    -- have bug
    -- Ideally, use queue for this.
    , addressInfo    :: Array IntroductoryMsg
    , address        :: Address
    , balance        :: Array (Tuple Color Coin)
    , transactions   :: Array TransactionSummarySerializable
    , periodId       :: Int
    , error          :: Maybe String
    }

init :: State
init =
    { route:          NotFound
    , socket:         Nothing
    , socketReady:    false
    , pendingActions: []
    , addressInfo:    []
    , address:        emptyAddress
    , balance:        []
    , transactions:   []
    , periodId:       0
    , error:          Nothing
    }
