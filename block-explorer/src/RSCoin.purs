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
       , isTransactionIncome
       , isTransactionExtensionOutcome
       , nominalDiffTimeToDateTime
       ) where

import Prelude                           (show, ($), map, (<<<))

import RSCoin.Core.Primitives            as C
import RSCoin.Core.Types                 as C
import RSCoin.Explorer.Web.Sockets.Types as E
import RSCoin.Explorer.Extended          as E
import Data.Types                        as T

import Data.Tuple (Tuple, snd, fst)
import Data.Maybe (Maybe (..))
import Data.Foldable (elem)

import Data.Time.Duration    (Seconds, fromDuration)
import Data.DateTime         (DateTime)
import Data.DateTime.Instant (instant, toDateTime)

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
addressToString = show <<< getPublicKey

getPublicKey :: C.Address -> T.PublicKey
getPublicKey (C.Address obj) = obj.getAddress

coinToColor :: C.Coin -> Int
coinToColor (C.Coin {getColor: C.Color c}) = c.getC

isTransactionIncome :: C.Address -> C.Transaction -> Boolean
isTransactionIncome addr (C.Transaction t) =
    elem (getPublicKey addr) <<< map (getPublicKey <<< fst) $ t.txOutputs

isTransactionExtensionOutcome :: C.Address -> E.TransactionExtension -> Boolean
isTransactionExtensionOutcome addr (E.TransactionExtension te) =
    elem (Just $ getPublicKey addr) <<< map (map getPublicKey) $ te.teInputAddresses

nominalDiffTimeToDateTime :: T.NominalDiffTime -> Maybe DateTime
nominalDiffTimeToDateTime (T.NominalDiffTime s) =
    map toDateTime <<< instant $ fromDuration s
