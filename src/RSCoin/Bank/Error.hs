-- | All possible runtime errors in Bank

module RSCoin.Bank.Error
       ( BankError (..)
       ) where

import           Control.Exception   (Exception (..))
import           Data.MessagePack    (MessagePack (fromObject, toObject))
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Data.Text.Buildable (Buildable (build))
import           Data.Typeable       (Typeable)

import           RSCoin.Core.Error   (rscExceptionFromException,
                                      rscExceptionToException)

data BankError
    = BEInternal Text    -- ^ Should not happen. Some invariant is broken.
    | BEBadRequest Text  -- ^ Someone requested something bad.
    deriving (Show, Typeable, Eq)

instance Exception BankError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable BankError where
    build (BEInternal m)     = "internal error: " <> build m
    build (BEBadRequest msg) = build msg

instance MessagePack BankError where
    toObject (BEInternal text)   = toObject (0::Int, text)
    toObject (BEBadRequest text) = toObject (1::Int, text)
    fromObject obj = do
        (i,payload) <- fromObject obj
        case (i :: Int) of
            0 -> BEInternal <$> fromObject payload
            1 -> BEBadRequest <$> fromObject payload
            _ -> Nothing
