{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- | This module describes errors that could happen in user-related
-- part of rscoin and provides some related functions to work with
-- them.

module RSCoin.User.Error
       ( UserError (..)
       , UserErrorLike (..)
       , eWrap
       ) where

import qualified RSCoin.User.Wallet  as W

import           Control.Exception   (Exception (..))
import           Control.Monad.Catch (MonadCatch, catch, throwM)
import qualified Data.Text           as T
import           Data.Typeable       (Typeable)
import           RSCoin.Core         (rscExceptionToException,
                                     rscExceptionFromException)

-- | This datatype describes all errors user side is 'aware of'.
-- Actually, the purpose of this datatype is wrapping all errors and
-- maybe show them in some pretty way. Any error that doesn't fit in
-- this definition should be considered unknown or unlikely to occur.
data UserError
    = StorageError W.WalletStorageError -- ^ Database errors
    | InputProcessingError T.Text       -- ^ Input processing errors
                                        -- (wrong user input)
    | NetworkError T.Text               -- ^ Errors related to network
    deriving (Show)

instance Exception UserError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

class (Exception a) => UserErrorLike a where
    toUserError :: a -> UserError

instance UserErrorLike W.WalletStorageError where
    toUserError = StorageError

-- | Errors that are UserErrorLike
data UserErrorWithClass =
    forall e. (Exception e, UserErrorLike e, Show e) => UserErrorWithClass e
    deriving (Typeable)

instance Show UserErrorWithClass where
    show (UserErrorWithClass a) = show a

instance Exception UserErrorWithClass

-- | Runs the monadic computation replacing any error that has
-- UserErrorLike instance with UserError.
eWrap :: (MonadCatch m) => m a -> m a
eWrap action =
    action `catch` localHandler -- FIXME it doesn't work :(
  where
    localHandler (UserErrorWithClass e) = throwM $ toUserError e
