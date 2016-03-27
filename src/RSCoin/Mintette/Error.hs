-- | All possible runtime errors in Mintette

module RSCoin.Mintette.Error
       ( MintetteError (..)
       ) where

import           Control.Exception (Exception)
import           Data.Typeable     (Typeable)

data MintetteError =
    MEInternal
    deriving (Show, Typeable)

instance Exception MintetteError
