-- | This module provides type class for named logger to embed logger name into context.

module RSCoin.Core.NamedLogging
        ( WithNamedLogger (..)
        ) where

import           Control.Monad.Trans        (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Network.MessagePack.Server (ServerT)

import           RSCoin.Core.NodeConfig     (NodeContext (..),
                                             defaultNodeContext)
import           RSCoin.Core.Primitives     (LoggerName)

-- | This class exists to remove boilerplate logging
-- by adding the logger's name to the environment
-- in each module.

class WithNamedLogger m where
    getLoggerFromContext :: m LoggerName

instance WithNamedLogger IO where
    getLoggerFromContext = return $ _loggerName defaultNodeContext

instance MonadIO m => WithNamedLogger (ServerT m) where
    getLoggerFromContext = liftIO $ getLoggerFromContext

instance (MonadIO m) => WithNamedLogger (ExceptT e m) where
    getLoggerFromContext = liftIO $ getLoggerFromContext
