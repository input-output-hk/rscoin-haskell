-- | All possible runtime errors in Mintette

module RSCoin.Mintette.Error
       ( MintetteError (..)
       ) where

import           Control.Exception (Exception)
import           Data.Text         (Text)
import           Data.Typeable     (Typeable)

data MintetteError =
    MEInternal Text
    deriving (Show, Typeable)

instance Exception MintetteError
