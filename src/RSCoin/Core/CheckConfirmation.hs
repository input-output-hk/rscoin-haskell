-- | Functions related to CheckConfirmation type

module RSCoin.Core.CheckConfirmation
       ( mkCheckConfirmation
       ) where

import           RSCoin.Core.Crypto     (SecretKey, derivePublicKey, sign)
import           RSCoin.Core.Primitives (AddrId, Transaction)
import           RSCoin.Core.Types      (ActionLogHead, CheckConfirmation (..))

mkCheckConfirmation :: SecretKey
                    -> Transaction
                    -> AddrId
                    -> ActionLogHead
                    -> CheckConfirmation
mkCheckConfirmation sk tx addrId ccHead =
    CheckConfirmation
    { ..
    }
  where
    ccMintetteKey = derivePublicKey sk
    ccMintetteSignature = sign sk (tx, addrId, ccHead)
