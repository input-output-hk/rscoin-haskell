module App.Routes where

import Prelude (($), (<<<), (<>), show, class Eq, eq, map)

import Data.Functor ((<$), (<$>))
import Data.Maybe (fromMaybe)
import Data.Generic (gEq)

import Data.I18N (Language (..), readBool, readLanguage)

import Control.Apply ((<*), (*>), (<*>))
import Control.Alternative ((<|>))

import Pux.Router (end, router, lit, str, param, Match)

import App.RSCoin (Hash (..), TransactionId, Address (..), addressToString,
                   PublicKey (..)) as T

type QueryParams =
    { color      :: Boolean
    , showToggle :: Boolean
    , language   :: Language
    }

noQueryParams :: QueryParams
noQueryParams =
    { color:      false
    , showToggle: false
    , language:   English
    }

data Route = Route Path QueryParams

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

match :: String -> Route
match url = fromMaybe notFoundRoute $ router url $
    Route Home <$> optionalParams <* end
    <|>
    Route <<< Transaction <<< T.Hash <$> (lit txLit *> str) <*> optionalParams <* end
    <|>
    Route <<< Address <<< mkAddress <$> (lit addressLit *> str) <*> optionalParams <* end
  where
    mkAddress addr = T.Address {getAddress: T.PublicKey addr}
    optionalParams :: Match QueryParams
    optionalParams = mkQuery <$> param "color" <*> param "toggle" <*> param "language"
    mkQuery c t l =
        { color: fromMaybe false $ readBool c
        , showToggle: fromMaybe false $ readBool t
        , language: fromMaybe English $ readLanguage l
        }

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
