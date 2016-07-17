module JsonDemo where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Tuple (Tuple (..))
import Data.Tuple.Nested (tuple3)

import Data.Argonaut.Printer (printJson)
import Data.Generic (class Generic)

import RSCoin.Explorer.Web.Sockets.Types as W
import RSCoin.Core.Primitives            (Address(Address), Coin(Coin), Transaction(Transaction))
import Data.Types                        (Hash(Hash), PublicKey(PublicKey), Rational(Rational))
import Serokell.Aeson.Helper (encodeJson)

data V = V { getV :: forall v. v } --this doesn't work either

logJS :: forall o e. (Generic o) => o -> Eff ( console :: CONSOLE | e) Unit
logJS = log <<< printJson <<< encodeJson

helper :: forall o e. (Generic o) => o -> Eff ( console :: CONSOLE | e) Unit
helper v = do
  --log $ show v   --these types don't have Show instances
    logJS v

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    let coin     = Coin {getColor: 0, getCoin: Rational "0.3242342"}
        key      = PublicKey "YblQ7+YCmxU/4InsOwSGH4Mm37zGjgy7CLrlWlnHdnM="
        hash     = Hash "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH+qtFzfEv46g="
        addr     = Address { getAddress:  key }
        tx       = Transaction {txInputs: [tuple3 hash 0 coin], txOutputs: [(Tuple addr coin)]}
        err      = W.ParseError "error"
        introMsg = W.IMAddressInfo addr
        aiMsg    = W.AIGetTransactions (Tuple 0 2)
        outMsg   = W.OMTxNumber 8 10
        helper'  = do
            helper coin
            helper key
            helper hash
            helper addr
            helper tx
            helper err
            helper introMsg
            helper aiMsg
            helper outMsg
    helper'
