module App.RSCoin
       ( module P
       , module S
       , module T
       , newAddress
       , emptyAddress
       , addressToString
       , coinToColor
       ) where

import Prelude                           (show, ($))

import RSCoin.Core.Primitives            as P
import RSCoin.Explorer.Web.Sockets.Types as S
import Data.Types                        as T

newAddress :: String -> P.Address
newAddress address = P.Address { getAddress: T.PublicKey address }

emptyAddress :: P.Address
emptyAddress = newAddress ""

addressToString :: P.Address -> String
addressToString (P.Address obj) = show obj.getAddress

coinToColor :: P.Coin -> Int
coinToColor (P.Coin {getColor:P.Color c}) = c.getC
