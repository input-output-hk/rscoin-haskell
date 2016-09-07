module App.RSCoin
       ( module C
       , module E
       , module T
       , CoinsMapExtended
       , TransactionExtended
       , CoinsMap
       , getTransactionId
       , getBalance
       , getCoins
       , newAddress
       , emptyAddress
       , addressToString
       , coinToColor
       ) where

import Prelude                           (show, ($), map)

import RSCoin.Core.Primitives            as C
import RSCoin.Core.Types                 as C
import RSCoin.Explorer.Web.Sockets.Types as E
import RSCoin.Explorer.Extended          as E
import Data.Types                        as T

import Data.Tuple (Tuple, snd)

type CoinsMap = Array (Tuple Int C.Coin)
type CoinsMapExtended = C.WithMetadata CoinsMap E.CoinsMapExtension
type TransactionExtended = C.WithMetadata C.Transaction E.TransactionExtension

getTransactionId :: TransactionExtended -> T.Hash
getTransactionId (C.WithMetadata {wmMetadata: E.TransactionExtension tId}) = tId.teId

getCoins :: CoinsMapExtended -> Array C.Coin
getCoins (C.WithMetadata cm) = map snd cm.wmValue

getBalance :: CoinsMapExtended -> T.CoinAmount
getBalance (C.WithMetadata {wmMetadata: E.CoinsMapExtension c }) = c._cmeTotal

newAddress :: String -> C.Address
newAddress address = C.Address { getAddress: T.PublicKey address }

emptyAddress :: C.Address
emptyAddress = newAddress ""

addressToString :: C.Address -> String
addressToString (C.Address obj) = show obj.getAddress

coinToColor :: C.Coin -> Int
coinToColor (C.Coin {getColor: C.Color c}) = c.getC
