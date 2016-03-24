-- | All possible runtime error in Bank

module Error
       ( BankError (..)
       ) where

import           Control.Exception (Exception)
import           Data.Text         (Text)
import           Data.Typeable     (Typeable)

data BankError =
    BEInternal Text
    deriving (Show, Typeable)

instance Exception BankError
