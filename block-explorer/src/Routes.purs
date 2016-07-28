module App.Routes where

import Prelude (($), (<<<), (<>), show)

import Data.Functor ((<$), (<$>))
import Data.Maybe (fromMaybe)

import Control.Apply ((<*), (*>))
import Control.Alternative ((<|>))

import Pux.Router (end, router, lit, str)

import App.RSCoin (Hash (..), TransactionId)

data Route
    = Home
    | Transaction TransactionId
    | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Home <$ end
    <|>
    Transaction <<< Hash <$> (lit txLit *> str) <* end

txLit :: String
txLit = "tx"

txUrl :: TransactionId -> String
txUrl tId = "/" <> txLit <> "/" <> show tId

