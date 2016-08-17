{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Re-export RSCoin.Timed.*

module RSCoin.Timed
        ( module Exports
        , StdGen
        , WorkMode
        , ContextArgument (..)
        , runEmulationMode
        , runEmulationMode_
        , runRealModeBank
        , runRealModeUntrusted
        ) where

import           RSCoin.Timed.Misc           as Exports
import           RSCoin.Timed.MonadRpc       as Exports
import           RSCoin.Timed.MonadTimed     as Exports
import           RSCoin.Timed.PureRpc        as Exports
import           RSCoin.Timed.Timed          as Exports
import           RSCoin.Timed.TimedIO        as Exports

import           Control.Lens                ((.~))
import           Control.Monad               (join)
import           Control.Monad.Catch         (MonadMask)
import           Control.Monad.Reader        (runReaderT)
import           Control.Monad.Trans         (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           System.Random               (StdGen, getStdGen)

import           RSCoin.Core.Crypto.Signing  (SecretKey)
import           RSCoin.Core.Logging         (LoggerName, WithNamedLogger,
                                              bankLoggerName)
import           RSCoin.Core.NodeConfig      (NodeContext, ctxLoggerName,
                                              defaultNodeContext,
                                              readDeployNodeContext)

class (MonadTimed m, MonadRpc m, MonadIO m, WithNamedLogger m,
       MonadMask m, MonadBaseControl IO m) => WorkMode m where

instance (MonadTimed m, MonadRpc m, MonadIO m, WithNamedLogger m,
          MonadMask m, MonadBaseControl IO m) => WorkMode m

-- | ContextArgument is passed to functions which need NodeContext. It
-- aggregates variety of ways to pass context.
data ContextArgument
    = CAExisting NodeContext     -- ^ Existing NodeContext -- will be used.
    | CADefaultLocation          -- ^ Context will be read from default location.
    | CACustomLocation FilePath  -- ^ Context will be read from given location.
    | CADefault                  -- ^ Default context will be used.
    deriving (Show)

mkNodeContext :: LoggerName
              -> Maybe SecretKey
              -> ContextArgument
              -> IO NodeContext
mkNodeContext loggerName sk ca =
    (ctxLoggerName .~ loggerName $) <$> mkNodeContextDo sk ca

mkNodeContextDo :: Maybe SecretKey -> ContextArgument -> IO NodeContext
mkNodeContextDo _ (CAExisting ctx) = pure ctx
mkNodeContextDo bankSecretKey CADefaultLocation =
    readDeployNodeContext bankSecretKey Nothing
mkNodeContextDo bankSecretKey (CACustomLocation p) =
    readDeployNodeContext bankSecretKey (Just p)
mkNodeContextDo _ CADefault = pure defaultNodeContext

runRealModeWithContext :: NodeContext -> MsgPackRpc a -> IO a
runRealModeWithContext nodeContext =
    runTimedIO . flip runReaderT nodeContext . runMsgPackRpc

runRealModeBank :: ContextArgument -> SecretKey -> MsgPackRpc a -> IO a
runRealModeBank ca bankSecretKey bankAction = do
    ctx <- mkNodeContext bankLoggerName (Just bankSecretKey) ca
    runRealModeWithContext ctx bankAction

runRealModeUntrusted :: LoggerName -> ContextArgument -> MsgPackRpc a -> IO a
runRealModeUntrusted logName ca nodeAction = do
    untrustedNodeContext <- mkNodeContext logName Nothing ca
    runRealModeWithContext untrustedNodeContext nodeAction

runEmulationMode :: MonadIO m => Maybe StdGen -> Delays -> PureRpc IO a -> m a
runEmulationMode genMaybe delays m =
    liftIO . join $ runPureRpc <$> getGen genMaybe <*> pure delays <*> pure m

runEmulationMode_ :: MonadIO m => Maybe StdGen -> Delays -> PureRpc IO () -> m ()
runEmulationMode_ genMaybe delays m =
    liftIO . join $ runPureRpc_ <$> getGen genMaybe <*> pure delays <*> pure m

getGen :: Maybe StdGen -> IO StdGen
getGen = maybe getStdGen pure
