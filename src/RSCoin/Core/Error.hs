{-# LANGUAGE ExistentialQuantification #-}
module RSCoin.Core.Error
       ( RSCoinError
       , rscExceptionToException
       , rscExceptionFromException
       ) where

import           Control.Exception   (Exception (..), SomeException)
import           Data.Typeable       (Typeable, cast)

data RSCoinError =
    forall e. Exception e => RSCoinError e
    deriving Typeable

instance Exception RSCoinError

instance Show RSCoinError where
    show (RSCoinError e) = show e

rscExceptionToException :: Exception e => e -> SomeException
rscExceptionToException = toException . RSCoinError

rscExceptionFromException :: Exception e => SomeException -> Maybe e
rscExceptionFromException x = do
    RSCoinError e <- fromException x
    cast x
