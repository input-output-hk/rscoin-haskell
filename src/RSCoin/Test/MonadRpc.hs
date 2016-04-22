{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}

-- This module contains RpcMonad providing RPC communication, 
-- and it's implementation using MessagePack.

module RSCoin.Test.MonadRpc
    ( Port
    , Host
    , Addr
    , MonadRpc
    , MsgPackRpc
    , runMsgPackRpc
    , RpcType
    , execClient
    , serve
    , Method(..)
    , Client(..)
    , method
    , call
    , S.Server
    , S.MethodType
    ) where

import qualified Data.ByteString            as BS 
import           Control.Monad.Catch           (MonadThrow, MonadCatch)
import           Control.Monad.State           (execState, put)
import           Control.Monad.Trans           (MonadIO, liftIO)
import           Data.IORef                    (newIORef, readIORef, writeIORef)
import           Data.Maybe                    (fromMaybe)

import qualified Network.MessagePack.Client as C
import qualified Network.MessagePack.Server as S

import           RSCoin.Test.MonadTimed        (TimedIO, MonadTimed)

import           Data.MessagePack.Object       (Object(..), MessagePack, 
                                                toObject)

type Port = Int

type Host = BS.ByteString

type Addr = (Host, Port)

-- | Defines protocol of RPC layer
class MonadRpc r where
    execClient :: MessagePack a => Addr -> Client a -> r a
    
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
    execClient (addr, port) (Client name args)  =  liftIO $ do
        box <- newIORef Nothing
        C.execClient addr port $ do
            -- note, underlying rpc accepts a single argument - [Object]
            res <- C.call name args
            liftIO . writeIORef box $ Just res
        fromMaybe (error "Aaa, execClient didn't return a value!") 
            <$> readIORef box
      where
        mkClient :: MessagePack a => Client a -> C.Client a
        mkClient (Client name args) = C.call name args 

    serve port methods  =  liftIO $ S.serve port (modifyMethod <$> methods)
      where
        modifyMethod :: Method IO -> S.Method IO
        modifyMethod m = let Method{..} = m
                         in  S.method methodName methodBody

    
-- * Client part 

-- | Creates a function call. It accepts function name and arguments
call :: RpcType t => String -> t
call name  =  rpcc name []

-- | Collects function name and arguments 
-- (it's MessagePack implementation is hiden, need our own)
class RpcType t where
    rpcc :: String -> [Object] -> t

instance (RpcType t, MessagePack p) => RpcType (p -> t) where
    rpcc name objs p  =  rpcc name $ toObject p : objs

-- | Keeps function name and arguments 
data Client a where
    Client :: MessagePack a => String -> [Object] -> Client a

instance MessagePack o => RpcType (Client o) where
    rpcc name args  =  Client name (reverse args)


-- * Server part

-- | Keeps method definition
data Method m  =  Method 
    { methodName :: String
    , methodBody :: [Object] -> m Object
    }

-- | Creates method available for RPC-requests.
--   It accepts method name (which would be refered by clients) 
--   and it's arguments
method :: S.MethodType m f => String -> f -> Method m
method name f  =  Method
    { methodName = name
    , methodBody = S.toBody f
    }

instance S.MethodType m f => S.MethodType m (m f) where
    toBody res args  =  res >>= \r -> S.toBody r args

instance Monad m => S.MethodType m Object where
    toBody res []  =  return res
    toBody _   _   =  error "Too many arguments!"
