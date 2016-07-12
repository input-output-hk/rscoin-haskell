-- | Functions related to CheckConfirmation type

module RSCoin.Core.CheckConfirmation
       ( mkCheckConfirmation
       , verifyCheckConfirmation
       ) where

import           RSCoin.Core.Crypto     (SecretKey, derivePublicKey, sign,
                                         verify)
import           RSCoin.Core.Primitives (AddrId, Transaction)
import           RSCoin.Core.Types      (ActionLogHead, CheckConfirmation (..),
                                         PeriodId)

mkCheckConfirmation :: SecretKey
                    -> Transaction
                    -> AddrId
                    -> ActionLogHead
                    -> PeriodId
                    -> CheckConfirmation
mkCheckConfirmation sk tx addrId ccHead ccPeriodId =
    CheckConfirmation
    { ..
    }
  where
    ccMintetteKey = derivePublicKey sk
    ccMintetteSignature = sign sk (tx, addrId, ccHead, ccPeriodId)

verifyCheckConfirmation :: CheckConfirmation
                        -> Transaction
                        -> AddrId
                        -> PeriodId
                        -> Bool
verifyCheckConfirmation CheckConfirmation{..} tx addrid periodId =
    ccPeriodId == periodId &&
    verify ccMintetteKey ccMintetteSignature (tx, addrid, ccHead, periodId)
