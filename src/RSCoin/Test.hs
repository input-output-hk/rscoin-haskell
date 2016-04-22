{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Re-export RSCoin.Test.*

module RSCoin.Test
       ( module Exports
       , WorkMode
       ) where

import           RSCoin.Test.MonadTimed         as Exports
import           RSCoin.Test.Timed              as Exports
import           RSCoin.Test.MonadRpc           as Exports
import           RSCoin.Test.PureRpc            as Exports

import           Control.Monad.Catch            (MonadThrow, MonadCatch)
import           Control.Monad.Trans            (MonadIO)

class (MonadTimed m, MonadRpc m, MonadIO m,
       MonadThrow m, MonadCatch m) => WorkMode m where

instance (MonadTimed m, MonadRpc m, MonadIO m,
       MonadThrow m, MonadCatch m) => WorkMode m

