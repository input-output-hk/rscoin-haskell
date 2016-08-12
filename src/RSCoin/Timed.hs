{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Re-export RSCoin.Timed.*

module RSCoin.Timed
        ( module Exports
        , StdGen
        , WorkMode
        , runEmulationMode
        , runEmulationMode_
        , runRealModeBank
        , runRealModeDefaultContext
        , runRealModeUntrusted
        ) where

import           RSCoin.Timed.Misc           as Exports
import           RSCoin.Timed.MonadRpc       as Exports
import           RSCoin.Timed.MonadTimed     as Exports
import           RSCoin.Timed.PureRpc        as Exports
import           RSCoin.Timed.Timed          as Exports
import           RSCoin.Timed.TimedIO        as Exports

import           Control.Monad               (join)
import           Control.Monad.Catch         (MonadMask)
import           Control.Monad.Reader        (runReaderT)
import           Control.Monad.Trans         (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           System.Random               (StdGen, getStdGen)

import           RSCoin.Core.Crypto.Signing  (SecretKey)
import           RSCoin.Core.Logging         (LoggerName, WithNamedLogger, bankLoggerName)
import           RSCoin.Core.NodeConfig      (NodeContext (..), defaultNodeContext,
                                              readDeployNodeContext)

class (MonadTimed m, MonadRpc m, MonadIO m, WithNamedLogger m,
       MonadMask m, MonadBaseControl IO m) => WorkMode m where

instance (MonadTimed m, MonadRpc m, MonadIO m, WithNamedLogger m,
          MonadMask m, MonadBaseControl IO m) => WorkMode m

runRealModeWithContext :: NodeContext -> MsgPackRpc a -> IO a
runRealModeWithContext nodeContext =
    runTimedIO . flip runReaderT nodeContext . runMsgPackRpc

runRealModeDefaultContext :: MsgPackRpc a -> IO a
runRealModeDefaultContext = runRealModeWithContext defaultNodeContext

runRealModeBank :: Maybe FilePath -> SecretKey -> MsgPackRpc a -> IO a
runRealModeBank confPath bankSecretKey bankAction = do
    bankNodeContext <- readDeployNodeContext (Just bankSecretKey) confPath
    runRealModeWithContext bankNodeContext { _ctxLoggerName = bankLoggerName } bankAction

runRealModeUntrusted :: LoggerName -> Maybe FilePath -> MsgPackRpc a -> IO a
runRealModeUntrusted logName confPath nodeAction = do
    untrustedNodeContext <- readDeployNodeContext Nothing confPath
    runRealModeWithContext untrustedNodeContext { _ctxLoggerName = logName } nodeAction

runEmulationMode :: MonadIO m => Maybe StdGen -> Delays -> PureRpc IO a -> m a
runEmulationMode genMaybe delays m =
    liftIO . join $ runPureRpc <$> getGen genMaybe <*> pure delays <*> pure m

runEmulationMode_ :: MonadIO m => Maybe StdGen -> Delays -> PureRpc IO () -> m ()
runEmulationMode_ genMaybe delays m =
    liftIO . join $ runPureRpc_ <$> getGen genMaybe <*> pure delays <*> pure m

getGen :: Maybe StdGen -> IO StdGen
getGen = maybe getStdGen pure
