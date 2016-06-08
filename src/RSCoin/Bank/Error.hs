-- | All possible runtime errors in Bank

module RSCoin.Bank.Error
       ( BankError (..)
       ) where

import           Control.Exception   (Exception (..))
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Data.Text.Buildable (Buildable (build))
import           Data.Typeable       (Typeable)
import           RSCoin.Core         (rscExceptionToException,
                                     rscExceptionFromException)

data BankError
    = BEInternal Text                     -- ^ Should not happen.
    | BEInconsistentResponse Text         -- ^ Inconsistency detected.
    deriving (Show, Typeable, Eq)

instance Exception BankError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable BankError where
    build (BEInternal m) = "internal error: " <> build m
    build (BEInconsistentResponse msg) = build msg
