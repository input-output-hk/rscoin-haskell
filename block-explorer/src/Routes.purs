module App.Routes where

import Prelude (($), (<<<), (<>), show, class Eq, eq)

import Data.Functor ((<$), (<$>))
import Data.Maybe (fromMaybe)
import Data.Generic (gEq)

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

instance eqRoute :: Eq Route where
    eq Home Home = true
    eq NotFound NotFound = true
    eq (Transaction tId1) (Transaction tId2) = eq tId1 tId2
    -- TODO: use `eq tId.txId tId2.txId` here to optimize
    eq (Address addr1) (Address addr2) = gEq addr1 addr2
    eq _ _ = false

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
