{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- This module contains RpcMonad providing RPC communication, 
-- and it's implementation using MessagePack.

module RSCoin.Test.MonadRpc
    (
    ) where

import qualified Data.ByteString            as BS 
import           Control.Monad.Trans           (MonadIO, liftIO)

import qualified Network.MessagePack.Client as C
import qualified Network.MessagePack.Server as S
import           Data.MessagePack

import           RSCoin.Test.MonadTimed (Timed, MonadTimed, RelativeToNow)

type Port = Int

type Hostname = BS.ByteString

type Addr = (Hostname, Port)

-- | Defines protocol of RPC layer
class MonadRpc r c s | r -> c, r -> s where
    execClient :: Addr -> c a -> r ()
    
    serve :: Port -> [S.Method r] -> r ()

-- | Same as MonadRpc, but we can set delays on per call basis.
--   MonadRpc also has specified delays, but only for whole network.
--   Default delay would be thrown away.
--   (another approach: stack this and default delays,
--    makes sense, as in RealRpc we still have default delays, even a little
--    but it can wreak a havok in pure implementation)
-- TODO: Do we actually need this?
-- class (MonadRpc r, MonadTimed r) => RpcDelayedMonad r where
--    execClientWithDelay  :: RelativeToNow -> Addr -> Client a -> r ()
--    serveWithDelay :: RelativeToNow -> Port -> [S.Method r] -> r ()


-- Implementation for MessagePack

newtype MsgPackRpc a  =  MsgPackRpc (Timed a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadTimed)

instance MonadRpc MsgPackRpc C.Client S.Server where 
    -- | Refers to MP.Client.execClient
    execClient (addr, port) cli  =  
        liftIO $ C.execClient addr port cli

     -- Refers to Server.serve
    serve  =  undefined 

-- Client part 

-- | Keeps function arguments 
-- (it's MessagePack implementation is hiden, need our own)
class RpcType t where
    rpcc :: String -> [Object] -> t

-- | Keeps function name and arguments
newtype Client a  =  Client (String, [Object])

instance RpcType (Client o) where
    rpcc  =  curry Client

instance (MessagePack p, RpcType t) => RpcType (p -> t) where
    rpcc name os p  =  rpcc name $ os ++ [toObject p]

call :: RpcType t => String -> t
call name  =  rpcc name []


-- Server part

newtype ServerT m a  =  ServerT { runServerT :: m a } 
    deriving (Functor, Applicative, Monad, MonadIO)

instance (Monad m, MessagePack o) => S.MethodType m (ServerT m o) where
    toBody m []  =  toObject <$> runServerT m
    toBody _   _   =  error "To many arguments passed"

method :: MethodType m f => String -> f- > Method m

