{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- | This module contains MonadRpc providing RPC communication,
-- and it's implementation using MessagePack.

module RSCoin.Timed.MonadRpc
       ( Port
       , Host
       , Addr
       , MonadRpc (serve, execClient, getPlatformLayout)
       , PlatformLayout (..)
       , MsgPackRpc
       , runMsgPackRpc
       , RpcType
       , execClientTimeout
       , Method(..)
       , Client(..)
       , method
       , call
       , S.Server
       , S.ServerT
       , S.MethodType
       , C.RpcError(..)
       , serverTypeRestriction0
       , serverTypeRestriction1
       , serverTypeRestriction2
       , serverTypeRestriction3
       , serverTypeRestriction5
       ) where

import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader        (MonadReader, ReaderT (..), ask,
                                              runReaderT)
import           Control.Monad.Trans         (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl, StM,
                                              liftBaseWith, restoreM)
import qualified Data.ByteString             as BS
import           Data.IORef                  (newIORef, readIORef, writeIORef)
import           Data.Maybe                  (fromMaybe)
import           Data.Time.Units             (TimeUnit, convertUnit)

import qualified Network.MessagePack.Client  as C
import qualified Network.MessagePack.Server  as S

import           RSCoin.Timed.MonadTimed     (MonadTimed (timeout))
import           RSCoin.Timed.TimedIO        (TimedIO)

import           Data.MessagePack.Object     (MessagePack, Object (..),
                                              toObject)

type Port = Int

type Host = BS.ByteString

type Addr = (Host, Port)

data PlatformLayout = PlatformLayout
    { getBankAddr   :: Addr
    , getNotaryAddr :: Addr
    } deriving (Show)

-- | Defines protocol of RPC layer
class MonadThrow r => MonadRpc r where
    execClient :: MessagePack a => Addr -> Client a -> r a

    serve :: Port -> [Method r] -> r ()

    getPlatformLayout :: r PlatformLayout

-- | Same as MonadRpc, but we can set delays on per call basis.
--   MonadRpc also has specified delays, but only for whole network.
--   Default delay would be thrown away.
--   (another approach: stack this and default delays,
--    makes sense, as in RealRpc we still have default delays, even a little
--    but it can be not convinient in pure implementation)
-- TODO: Do we actually need this?
-- class (MonadRpc r, MonadTimed r) => RpcDelayedMonad r where
--    execClientWithDelay  :: RelativeToNow -> Addr -> Client a -> r ()
--    serveWithDelay :: RelativeToNow -> Port -> [S.Method r] -> r ()


-- Implementation for MessagePack

newtype MsgPackRpc a = MsgPackRpc
    { runMsgPackRpc :: ReaderT PlatformLayout TimedIO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow,
                MonadCatch, MonadMask, MonadTimed, MonadReader PlatformLayout)

instance MonadBaseControl IO MsgPackRpc where
    type StM MsgPackRpc a = a

    liftBaseWith f = MsgPackRpc $ liftBaseWith $ \g -> f $ g . runMsgPackRpc

    restoreM = MsgPackRpc . restoreM

instance MonadRpc MsgPackRpc where
    execClient (addr, port) (Client name args) = liftIO $ do
        box <- newIORef Nothing
        C.execClient addr port $ do
            -- note, underlying rpc accepts a single argument - [Object]
            res <- C.call name args
            liftIO . writeIORef box $ Just res
        fromMaybe (error "Aaa, execClient didn't return a value!")
            <$> readIORef box

    serve port methods = S.serve port $ convertMethod <$> methods
      where
        convertMethod :: Method MsgPackRpc -> S.Method MsgPackRpc
        convertMethod Method{..} = S.method methodName methodBody

    getPlatformLayout = ask

instance MonadRpc m => MonadRpc (ReaderT r m) where
    execClient addr cli = lift $ execClient addr cli

    serve port methods = ReaderT $
                            \r ->  serve port (convert r <$> methods)
      where
        convert :: Monad m => r -> Method (ReaderT r m) -> Method m
        convert r Method {..} =
            Method methodName (flip runReaderT r . methodBody)

    getPlatformLayout = lift getPlatformLayout

execClientTimeout
    :: (MonadTimed m, MonadRpc m, MessagePack a, TimeUnit t)
    => t -> Addr -> Client a -> m a
execClientTimeout (convertUnit -> t) addr = timeout t . execClient addr

-- * Client part

-- | Creates a function call. It accepts function name and arguments
call :: RpcType t => String -> t
call name = rpcc name []

-- | Collects function name and arguments
-- (it's MessagePack implementation is hiden, need our own)
class RpcType t where
    rpcc :: String -> [Object] -> t

instance (RpcType t, MessagePack p) => RpcType (p -> t) where
    rpcc name objs p = rpcc name $ toObject p : objs

-- | Keeps function name and arguments
data Client a where
    Client :: MessagePack a => String -> [Object] -> Client a

instance MessagePack o => RpcType (Client o) where
    rpcc name args = Client name (reverse args)

-- * Server part

-- | Keeps method definition
data Method m = Method
    { methodName :: String
    , methodBody :: [Object] -> m Object
    }

-- | Creates method available for RPC-requests.
--   It accepts method name (which would be refered by clients)
--   and it's body
method :: S.MethodType m f => String -> f -> Method m
method name f = Method
    { methodName = name
    , methodBody = S.toBody f
    }

instance S.MethodType MsgPackRpc f => S.MethodType MsgPackRpc (MsgPackRpc f)
   where
    toBody res args = res >>= \r -> S.toBody r args

instance Monad m => S.MethodType m Object where
    toBody res [] = return res
    toBody _   _  = error "Too many arguments!"

-- | Helps restrict method type
serverTypeRestriction0 :: Monad m => m (S.ServerT m a -> S.ServerT m a)
serverTypeRestriction0 = return id

serverTypeRestriction1 :: Monad m => m ((b -> S.ServerT m a) -> (b -> S.ServerT m a))
serverTypeRestriction1 = return id

serverTypeRestriction2 :: Monad m => m ((c -> b -> S.ServerT m a) -> (c -> b -> S.ServerT m a))
serverTypeRestriction2 = return id

serverTypeRestriction3 :: Monad m => m ((d -> c -> b -> S.ServerT m a) -> (d -> c -> b -> S.ServerT m a))
serverTypeRestriction3 = return id

serverTypeRestriction5
    :: Monad m
    => m ((f -> e -> d -> c -> b -> S.ServerT m a) -> (f -> e -> d -> c -> b -> S.ServerT m a))
serverTypeRestriction5 = return id
