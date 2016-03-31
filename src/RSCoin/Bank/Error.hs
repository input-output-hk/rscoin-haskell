-- | All possible runtime errors in Bank

module RSCoin.Bank.Error
       ( BankError (..)
       ) where

import           Control.Exception (Exception)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Data.Text.Buildable (Buildable (build))
import qualified Data.Text.Format    as F
import           Data.Typeable       (Typeable)

data BankError =
    BEInternal Text
    deriving (Show, Typeable, Eq)

instance Exception BankError

instance Buildable BankError where
    build (BEInternal m) = "internal error: " <> build m
