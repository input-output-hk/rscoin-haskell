{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- This module contains RpcMonad providing RPC communication, 
-- and it's implementation using MessagePack.

module RSCoin.Test.MonadRpc
    
    where

import qualified Data.ByteString            as BS 
import           Control.Monad.Catch           (MonadThrow, MonadCatch)
import           Control.Monad.State           (State, execState, put)
import           Control.Monad.Identity        (runIdentity)
import           Control.Monad.Trans           (MonadIO, liftIO)
import           Data.Maybe                    (fromJust)
import           Data.Proxy                    (Proxy(..))
import           Control.Lens                  (Prism', prism, re
                                               , (^.))


import qualified Network.MessagePack.Client as C
import qualified Network.MessagePack.Server as S

import           RSCoin.Test.MonadTimed (TimedIO, MonadTimed, RelativeToNow)

import           Data.MessagePack.Object       (Object(..), MessagePack, toObject, fromObject)

type Port = Int

type Hostname = BS.ByteString

type Addr = (Hostname, Port)

-- | Defines protocol of RPC layer
class MonadRpc r where
    type Cli r :: * -> *

    execClient :: Addr -> Cli r a -> r ()
    
    serve :: Port -> [Method IO] -> r ()

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

newtype MsgPackRpc a  =  MsgPackRpc { runMsgPackRpc :: (TimedIO a) }
    deriving (Functor, Applicative, Monad, MonadIO, 
              MonadThrow, MonadCatch, MonadTimed)

instance MonadRpc MsgPackRpc where 
    type Cli MsgPackRpc  =  C.Client

    execClient (addr, port) cli  =  
        liftIO $ C.execClient addr port cli

    serve port methods  =  liftIO $ S.serve port (modifyMethod <$> methods)
      where
        modifyMethod :: Method IO -> S.Method IO
        modifyMethod m = let Method{..} = m
                         in  S.method methodName methodBody
    
-- Client part 

-- | Keeps function name and arguments 
-- (it's MessagePack implementation is hiden, need our own)
class RpcType t where
    rpcc :: String -> [Object] -> t

instance MessagePack o => RpcType (C.Client o) where
    rpcc name objs  =  C.call name objs

instance (RpcType t, MessagePack p) => RpcType (p -> t) where
    rpcc name objs p  =  rpcc name $ toObject p : objs

instance MessagePack r => RpcType (String, [Object], Proxy r) where
    rpcc name objs  =  (name, objs, Proxy)

call :: RpcType t => String -> t
call name  =  rpcc name []

-- Server part

data Method m  =  Method 
    { methodName :: String
    , methodBody :: [Object] -> m Object
    }

method :: S.MethodType m f => String -> f -> Method m
method name f  =  Method
    { methodName = name
    , methodBody = S.toBody f
    }

newtype ServerT m a  =  ServerT { runServerT :: m a } 
    deriving (Functor, Applicative, Monad, MonadIO)

instance (Monad m, MessagePack o) => S.MethodType m (ServerT m o) where
    toBody m []  =  toObject <$> runServerT m
    toBody _ _   =  error "Too many arguments passed"

instance S.MethodType m f => S.MethodType m (m f) where
    toBody res args  =  res >>= \r -> S.toBody r args

instance Monad m => S.MethodType m Object where
    toBody res []  =  return res
    toBody _   _   =  error "!!"
