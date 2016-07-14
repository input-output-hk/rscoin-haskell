module JsonDemo where

import Prelude

import Control.Monad.Eff.Console (log)

import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson)
import Data.Argonaut.Printer       (printJson)

import RSCoin.Explorer.Web.Sockets.Types as T
import RSCoin.Core.Primitives            as T
import Data.Types                        as T

logJS = log <<< printJson <<< encodeJson

main = do
  let c        = T.Coin {getColor: 1, getCoin: T.Rational "thisisastring"}
      tr       = T.Transaction {txInputs: [], txOutputs: []}
      adr      = T.Address { getAddress: T.PublicKey "YblQ7+YCmxU/4InsOwSGH4Mm37zGjgy7CLrlWlnHdnM=" }
      introAdr = T.IMAddressInfo adr
  logJS c