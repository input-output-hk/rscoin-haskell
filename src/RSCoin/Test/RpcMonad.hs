{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This module contains RpcMonad providing RPC communication, 
-- and it's implementation using MessagePack.

module RSCoin.Test.RpcMonad
    (
    ) where

import qualified Data.ByteString            as BS 

import qualified Network.MessagePack.Client as C
import qualified Network.MessagePack.Server as S
import           Data.MessagePack

import           RSCoin.Test.MonadTimed (Timed, MonadTimed, RelativeToNow)

type Port = Int

data Addr = Addr
    { host :: String
    , port :: Port
    } deriving (Eq, Ord, Read, Show)

-- | Defines protocol of RPC layer
class RpcMonad r where
    call :: Addr -> Client a -> r ()
    
    serve :: Port -> [S.Method r] -> r ()

-- | Same as RpcMonad, but we can set delays on per call basis.
--   RpcMonad also has specified delays, but only for whole network.
--   Default delay would be thrown away.
--   (another approach: stack this and default delays,
--    makes sense, as in RealRpc we still have default delays, even a little
--    but it can create a havok in pure implementation)
-- TODO: Do we actually need this?
class (RpcMonad r, MonadTimed r) => RpcDelayedMonad r where
    callWithDelay  :: RelativeToNow -> Addr -> Client a -> r ()
    serveWithDelay :: ()


-- Implementation for MessagePack

newtype RealRpc a  =  RealRpc (Timed a)
    deriving (MonadTimed)

instance RpcMonad RealRpc where 
    -- | Refers to MP.Client.execClient
    call _ (addr, port) (funcName, args)  =  
        let cli = undefined
        in  C.execClient (BS.pack addr) port cli

     -- Refers to Server.serve
    serve  =  undefined 

-- Client part 

-- | Keeps function arguments 
-- (it's MessagePack implementation is hiden, need our own)
class RpcType t where
    rpcc :: [Object] -> t

-- | Keeps function name and arguments
type Client a  =  (String, [Object])

instance RpcType (Client a) where
    rpcc  =  id

instance (MessagePack p, RpcType t) => RpcType (p -> t) where
    rpcc os p  =  rpcc $ os ++ toObject p

-- | Creates client, replacement for to MP.Client.call
mkClient :: RpcType t => String -> t -> Client a
mkClient name args  =  (name, rpcc args)

 
