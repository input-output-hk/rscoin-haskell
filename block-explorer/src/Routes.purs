module App.Routes where

import Prelude (($))

import Data.Functor ((<$), (<$>))
import Data.Maybe (fromMaybe)

import Control.Apply ((<*), (*>))
import Control.Alternative ((<|>))

import Pux.Router (end, router, lit, str)

data Route = Home | Transaction String | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Home <$ end
    <|>
    Transaction <$> (lit "tx" *> str) <* end
