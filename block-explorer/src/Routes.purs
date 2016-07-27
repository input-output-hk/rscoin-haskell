module App.Routes where

import Prelude (($), (<<<))

import Data.Functor ((<$), (<$>))
import Data.Maybe (fromMaybe)

import Control.Apply ((<*), (*>))
import Control.Alternative ((<|>))

import Pux.Router (end, router, lit, str)

import App.RSCoin (Address (..), PublicKey (..))

data Route
    = Home
    | Transaction Address
    | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Home <$ end
    <|>
    Transaction <<< mkAddress <$> (lit "tx" *> str) <* end
  where
    mkAddress addr = Address {getAddress: PublicKey addr}
