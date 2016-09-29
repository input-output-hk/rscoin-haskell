module App.Types
       ( module RSCoin
       , module Color
       , Action (..)
       , SearchQuery (..)
       , init
       , State
       , queryToString
       , searchQueryAddress
       ) where

import Prelude                     (show, class Eq, ($))

import App.Routes                  (Path (NotFound))
import App.Connection              (Connection, Action) as C
import App.RSCoin                  as RSCoin
import App.RSCoin                  (Coin, Address,
                                    TransactionExtended,
                                    HBlockExtension,
                                    getTransactionId,
                                    addressToString, CoinsMapExtended)
import Data.Color                  as Color
import Data.I18N                   (Language (..))

import Data.Monoid                 (mempty)
import Data.Maybe                  (Maybe (..))
import Data.Generic                (gEq)
import Data.Tuple                  (Tuple (..))
import Data.DateTime               (DateTime)
import Data.DateTime.Instant       (instant, toDateTime)
import Data.Time.Duration          (Milliseconds (..))

import Serokell.Data.Maybe         (unsafeFromJust)


data Action
    = PageView Path
    | SocketAction C.Action
    | SearchQueryChange String
    | SearchButton
    | DismissError
    | ColorToggle
    | LanguageSet Language
    | UpdateClock
    | SetClock DateTime
    -- NOTE: Int should be good enough for frontpage
    | ExpandTransactions
    | ExpandTransactionsGlobal
    | ExpandBlockchain
    | PaginationUpdate String
    | PaginationLeftBlocks
    | PaginationRightBlocks
    | PaginationLeftTransactions
    | PaginationRightTransactions
    | PaginationSearchBlocks
    | PaginationSearchTransactions
    | Nop

data SearchQuery
    = SQAddress Address
    | SQTransaction TransactionExtended

queryToString :: SearchQuery -> String
queryToString (SQAddress addr) = addressToString addr
queryToString (SQTransaction t) = show $ getTransactionId t

instance eqSearchQeuery :: Eq SearchQuery where
    eq (SQAddress addr1) (SQAddress addr2) = gEq addr1 addr2
    -- TODO: use `eq tId.txId tId2.txId` here to optimize
    eq (SQTransaction tId1) (SQTransaction tId2) = gEq tId1 tId2
    eq _ _ = false

searchQueryAddress :: SearchQuery -> Maybe Address
searchQueryAddress (SQAddress addr) = Just addr
searchQueryAddress _ = Nothing

type State =
    { route            :: Path
    , socket           :: Maybe C.Connection
    , socketReady      :: Boolean
    , pendingActions   :: Array Action
    , queryInfo        :: Maybe SearchQuery
    , searchQuery      :: String
    , balance          :: Maybe CoinsMapExtended
    , txNumber         :: Maybe Int
    , transactions     :: Array TransactionExtended
    , blocks           :: Array HBlockExtension
    , periodId         :: Int
    , error            :: Maybe String
    , colors           :: Boolean
    , language         :: Language
    , now              :: DateTime
    , paginationPage   :: String
    , paginationExpand :: Boolean
    }

init :: State
init =
    { route:            NotFound
    , socket:           Nothing
    , socketReady:      false
    , pendingActions:   []
    , queryInfo:        Nothing
    , searchQuery:      mempty
    , balance:          Nothing
    , txNumber:         Nothing
    , transactions:     mempty
    , blocks:           mempty
    , periodId:         0
    , error:            Nothing
    , colors:           false
    , language:         Japanese
    , now:              toDateTime $ unsafeFromJust $ instant $ Milliseconds 0.0
    , paginationPage:   mempty
    , paginationExpand: true
    }
