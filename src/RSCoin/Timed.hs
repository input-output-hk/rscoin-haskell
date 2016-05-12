{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Re-export RSCoin.Timed.*

module RSCoin.Timed
       ( module Exports
       , WorkMode
       , runRealMode, runRealMode_
       , runEmulationMode
       ) where

import           RSCoin.Timed.MonadTimed as Exports
import           RSCoin.Timed.Timed      as Exports
import           RSCoin.Timed.TimedIO    as Exports
import           RSCoin.Timed.MonadRpc   as Exports
import           RSCoin.Timed.PureRpc    as Exports
import           RSCoin.Timed.Misc       as Exports

import           Control.Monad.Catch     (MonadMask)
import           Control.Monad.Trans     (MonadIO)
import           System.Random           (StdGen)

class (MonadTimed m, MonadRpc m, MonadIO m,
       MonadMask m) => WorkMode m where

instance (MonadTimed m, MonadRpc m, MonadIO m,
       MonadMask m) => WorkMode m

runRealMode :: MsgPackRpc a -> IO a
runRealMode = runTimedIO . runMsgPackRpc

runRealMode_ :: MsgPackRpc a -> IO ()
runRealMode_ = runTimedIO_ . runMsgPackRpc

runEmulationMode :: StdGen -> Delays -> PureRpc IO () -> IO ()
runEmulationMode = runPureRpc
