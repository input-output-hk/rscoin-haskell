-- | All possible runtime errors in Mintette

module RSCoin.Mintette.Error
       ( MintetteError (..)
       ) where

import           Control.Exception (Exception)
import           Data.Text         (Text)
import           Data.Typeable     (Typeable)

import           RSCoin.Core       (PeriodId)

data MintetteError
    = MEInternal Text                     -- ^ Should not happen.
    | MEInactive                          -- ^ Mintette is not active right now.
    | MEPeriodMismatch PeriodId PeriodId  -- ^ PeriodId expected by mintette is
                                          -- different from the one expected by somebody.
    | MEInvalidTxSums                     -- ^ Mintette received transaction with invalid sums.
    | MEInconsistentRequest Text          -- ^ Inconsistency detected.
    | MEDoubleSpending                    -- ^ Double spending detected.
    | MEInvalidSignature                  -- ^ Signature check failed.
    | MENotConfirmed                      -- ^ Can't deduce that transaction was confirmed.
    deriving (Show,Typeable)

instance Exception MintetteError
