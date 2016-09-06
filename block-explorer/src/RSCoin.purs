module App.RSCoin
       ( module C
       , module E
       , module T
       , CoinsMapExtended
       , TransactionExtended
       , getTransactionId
       , newAddress
       , emptyAddress
       , addressToString
       , coinToColor
       ) where

import Prelude                           (show, ($))

import RSCoin.Core.Primitives            as C
import RSCoin.Core.Types                 as C
import RSCoin.Explorer.Web.Sockets.Types as E
import RSCoin.Explorer.Extended          as E
import Data.Types                        as T

import Data.Tuple (Tuple)

type CoinsMapExtended = C.WithMetadata (Array (Tuple Int C.Coin)) E.CoinsMapExtension
type TransactionExtended = C.WithMetadata C.Transaction E.TransactionExtension

getTransactionId :: TransactionExtended -> T.Hash
getTransactionId (C.WithMetadata {wmMetadata: E.TransactionExtension tId}) = tId.teId

newAddress :: String -> C.Address
newAddress address = C.Address { getAddress: T.PublicKey address }

emptyAddress :: C.Address
emptyAddress = newAddress ""

addressToString :: C.Address -> String
addressToString (C.Address obj) = show obj.getAddress

coinToColor :: C.Coin -> Int
coinToColor (C.Coin {getColor: C.Color c}) = c.getC
