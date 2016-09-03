module JsonDemo where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.Tuple.Nested (tuple4)

import Data.Argonaut.Printer (printJson)
import Data.Generic (class Generic)

import App.RSCoin as W
import Serokell.Aeson.Helper (encodeJson)

data V = V { getV :: forall v. v } --this doesn't work either

logJS :: forall o e. (Generic o) => o -> Eff ( console :: CONSOLE | e) Unit
logJS = log <<< printJson <<< encodeJson

helper :: forall o e. (Generic o) => o -> Eff ( console :: CONSOLE | e) Unit
helper v = do
  --log $ show v   --these types don't have Show instances
    logJS v

coin       = W.Coin {getColor: color, getCoin: coinAmount}
color      = W.Color {getC: 7}
coinAmount = W.CoinAmount "0.3242342"
key        = W.PublicKey "YblQ7+YCmxU/4InsOwSGH4Mm37zGjgy7CLrlWlnHdnM="
hash       = W.Hash "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH+qtFzfEv46g="
addr       = W.Address { getAddress:  key }
coinsMap   = W.CoinsMapSummary { _cmsCoinsMap: [Tuple 1 coin], _cmsCoinAmount: coinAmount }
tx         = W.TransactionSummary {txsId: hash, txsInputs: [tuple4 hash 5 coin $ Just addr], txsOutputs: [(Tuple addr coin)], txsInputsSum: coinsMap, txsOutputsSum: coinsMap }
err        = W.LogicError "error"
setAddr    = W.CMSetAddress addr
getTx      = W.CMGetTransaction hash
smart      = W.CMSmart "bla"
aiMsg      = W.AIGetTransactions (Tuple 0 2)
outMsg     = W.OMTxNumber addr 8 8

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    helper coin
    helper key
    helper hash
    helper addr
    helper tx
    helper err
    helper setAddr
    helper getTx
    helper smart
    helper aiMsg
    helper outMsg
