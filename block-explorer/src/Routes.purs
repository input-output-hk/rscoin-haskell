module App.Routes where

import Prelude (($), (<<<), (<>), show)

import Data.Functor ((<$), (<$>))
import Data.Maybe (fromMaybe)

import Control.Apply ((<*), (*>))
import Control.Alternative ((<|>))

import Pux.Router (end, router, lit, str)

import App.RSCoin (Hash (..), TransactionId, Address (..), addressToString,
                   PublicKey (..)) as T

data Route
    = Home
    | Transaction T.TransactionId
    | Address T.Address
    | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Home <$ end
    <|>
    Address <<< mkAddress <$> (lit addressLit *> str) <* end
    <|>
    Transaction <<< T.Hash <$> (lit txLit *> str) <* end
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
