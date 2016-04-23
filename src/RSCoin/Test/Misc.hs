module RSCoin.Test.Misc
    where

import Control.Exception.Base (SomeException)
import Control.Monad.Catch    (MonadMask, mask, throwM, catch)

-- implementation of bracket for any MonadMask
bracket' :: MonadMask m
         => m a         -- ^ computation to run first (\"acquire resource\")
         -> (a -> m b)  -- ^ computation to run last (\"release resource\")
         -> (a -> m c)  -- ^ computation to run in-between
         -> m c         -- returns the value from the in-between computation
bracket' before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r
  where
    onException io what = io `catch` \e -> what >> throwM (e :: SomeException)


