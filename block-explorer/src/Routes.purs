module App.Routes where

import Prelude (($), (<<<), (<>), show, class Eq, eq, map, (/=), not)

import Data.Functor ((<$), (<$>))
import Data.Maybe (fromMaybe, Maybe (..))
import Data.Generic (gEq)
import Data.String (joinWith)
import Data.Array (catMaybes, null)

import Data.I18N (Language (..), readBool, readLanguage)

import Control.Apply ((<*), (*>), (<*>))
import Control.Alternative ((<|>))

import Pux.Router (end, router, lit, str, param, Match)

import App.RSCoin (Hash (..), TransactionId, Address (..), addressToString,
                   PublicKey (..)) as T

type QueryParams =
    { color       :: Boolean
    , colorOption :: Boolean
    , language    :: Language
    }

noQueryParams :: QueryParams
noQueryParams =
    { color:       false
    , colorOption: false
    , language:    English
    }

data Route = Route Path QueryParams

getPath :: Route -> Path
getPath (Route p _) = p

getQueryParams :: Route -> QueryParams
getQueryParams (Route _ q) = q

instance eqRoute :: Eq Route where
    eq (Route p1 _) (Route p2 _) = eq p1 p2

notFoundRoute :: Route
notFoundRoute = Route NotFound noQueryParams

data Path
    = Home
    | Transaction T.TransactionId
    | Address T.Address
    | NotFound

instance eqPath :: Eq Path where
    eq Home Home = true
    eq NotFound NotFound = true
    eq (Transaction tId1) (Transaction tId2) = eq tId1 tId2
    -- TODO: use `eq tId.txId tId2.txId` here to optimize
    eq (Address addr1) (Address addr2) = gEq addr1 addr2
    eq _ _ = false

toUrl :: Route -> String
toUrl (Route p q) =
    if not $ null queries
        then path p <> "?" <> joinWith "&" queries
        else ""
  where
    path Home = homeUrl
    path (Transaction tId) = txUrl tId
    path (Address addr) = addressUrl addr
    path NotFound = homeUrl
    -- TODO: use Generic for generating this list
    queries = catMaybes
        [ if q.color
            then Just $ query "color" $ show q.color
            else Nothing
        , if q.colorOption
            then Just $ query "cOption" $ show q.colorOption
            else Nothing
        , if q.language /= English
            then Just $ query "language" $ show q.language
            else Nothing
        ]
    query s v = s <> "=" <> v

match :: String -> Path
match url = fromMaybe NotFound $ router url $
    Home <$ end
    <|>
    Transaction <<< T.Hash <$> (lit txLit *> str) <* end
    <|>
    Address <<< mkAddress <$> (lit addressLit *> str) <* end
  where
    mkAddress addr = T.Address {getAddress: T.PublicKey addr}

addressLit :: String
addressLit = "address"

txLit :: String
txLit = "tx"

addressUrl :: T.Address -> String
addressUrl address = litUrl addressLit <> T.addressToString address

txUrl :: T.TransactionId -> String
txUrl tId = litUrl txLit <> show tId

homeUrl :: String
homeUrl = "/"

litUrl :: String -> String
litUrl lit = "/" <> lit <> "/"
