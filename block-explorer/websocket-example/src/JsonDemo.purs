module JsonDemo where

import Prelude

import Control.Monad.Eff.Console (log)

import Data.Tuple (Tuple (..))
import Data.Tuple.Nested (tuple3)

import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson)
import Data.Argonaut.Printer       (printJson)

import RSCoin.Explorer.Web.Sockets.Types as W
import RSCoin.Core.Primitives            as C
import Data.Types                        as C

data V = V { getV :: forall v. v } --this doesn't work either

logJS = log <<< printJson <<< encodeJson

helper v = do
  --log $ show v   --these types don't have Show instances
    logJS v

main = do
  let coin     = C.Coin {getColor: 0, getCoin: C.Rational "0.3242342"}
      key      = C.PublicKey "YblQ7+YCmxU/4InsOwSGH4Mm37zGjgy7CLrlWlnHdnM="
      hash     = C.Hash "\SOWQ\192&\229C\178\232\171.\176`\153\218\161\209\229\223Gw\143w\135\250\171E\205\241/\227\168"
      addr     = C.Address { getAddress:  key }
      tx       = C.Transaction {txInputs: [tuple3 hash 0 coin], txOutputs: [(Tuple addr coin)]}
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

