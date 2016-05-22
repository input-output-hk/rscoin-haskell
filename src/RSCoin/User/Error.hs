{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- | This module describes errors that could happen in user-related
-- part of rscoin and provides some related functions to work with
-- them.

module RSCoin.User.Error
       ( UserError (..)
       , isWalletSyncError
       , UserLogicError (..)
       , UserErrorLike (..)
       , eWrap
       ) where

import           RSCoin.Core         (rscExceptionFromException,
                                      rscExceptionToException)
import qualified RSCoin.User.Wallet  as W

import           Control.Exception   (Exception (..), SomeException)
import           Control.Monad.Catch (MonadCatch, catch, throwM)
import qualified Data.Text           as T
import           Data.Typeable       (Typeable)

-- | This datatype describes all errors user side is 'aware of'.
-- Actually, the purpose of this datatype is wrapping all errors and
-- maybe show them in some pretty way. Any error that doesn't fit in
-- this definition should be considered unknown or unlikely to occur.
data UserError
    = StorageError W.WalletStorageError -- ^ Database errors
    | InputProcessingError T.Text       -- ^ Input processing errors
                                        -- (wrong user input)
    | WalletSyncError T.Text            -- ^ Wallet is out of sync
    | NetworkError T.Text               -- ^ Errors related to network
    deriving (Show)

isWalletSyncError :: SomeException -> Bool
isWalletSyncError e =
    case fromException e of
        Just (WalletSyncError _) -> True
        _ -> False

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

-- | This datatype describes errors that can be produced during
-- 'RSCoin.User.Logic.validateTransaction' function execution.
data UserLogicError
    = MajorityRejected T.Text
    | MajorityFailedToCommit Word Word
    deriving (Eq, Show, Typeable)

instance Exception UserLogicError where
    toException   = rscExceptionToException
    fromException = rscExceptionFromException

-- | Runs the monadic computation replacing any error that has
-- UserErrorLike instance with UserError.
eWrap :: (MonadCatch m) => m a -> m a
eWrap action =
    action `catch` localHandler -- FIXME it doesn't work :(
  where
    -- TODO: catch RSCoinError here!
    localHandler (UserErrorWithClass e) = throwM $ toUserError e
