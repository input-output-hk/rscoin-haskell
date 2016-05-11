-- | Errors in full testing.

module Test.RSCoin.Full.Error
       ( TestError (..)
       ) where

import           Control.Exception (Exception)
import           Data.Text         (Text)
import           Data.Typeable     (Typeable)

data TestError =
    TestError Text
    deriving (Show,Typeable,Eq)

instance Exception TestError
