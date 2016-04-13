{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Emulation 
       ( emulation
       , start
       , Port(..)
       , Addr(..)
       , Mcs
       , Timestamp
       , Emulation(..)
       


       ) where

import          Data.Map                    as Map
import          Control.Applicative         ((<$>))
import          Control.Monad               (void)
import          Control.Monad.Random        (RandT, evalRandT)
import          Control.Monad.Trans         (lift, liftIO)
import          Control.Monad.Trans.Maybe   (MaybeT(..), runMaybeT)
import          Data.Default                (Default, def)
import          System.Random               (StdGen, mkStdGen)
import          Control.Lens                ((+~), (&), (%~), (?~), (^.), 
                                             makeLenses, at)
import          Data.MessagePack.Object     (Object(..), toObject, fromObject)

type Port = Int

data Addr = Addr
    { host :: String
    , port :: Port
    } deriving (Eq, Ord, Read, Show)

-- something measured in microseconds
type Mcs = Int

type Timestamp = Mcs

type FunCoord = (Addr, String)

-- TODO: replace with actual arguments, make safe functions e.t.c.
type Arguments = Int
type Result = Int

class RpcType r where
  rpcc :: String -> [Object] -> r

data Event 
    = Invoke (Emulation -> IO Emulation)
    | RpcRec    
        { sender   :: Addr
        , rpcId    :: Int
        , funCoord :: FunCoord
        , args     :: Arguments
        }
    | RpcAns
        { rpcId    :: Int 
        , answer   :: Result
        }

data Delays = Delays 
    { evalDelay :: Addr -> Event -> RandT StdGen Maybe Mcs
            -- Just delay if net packet delivered successfully
            -- Nothing otherwise
    }

data Emulation = Emulation
    { _events     :: Map.Map Timestamp (Addr, Event)  
        --TODO: need priority queue here (or at least multimap)
        --      nice if we can zip infinite queues to use continuous scenarios
    , _listeners  :: Map.Map FunCoord (Arguments -> Result)
    , _rpcCounter :: Int
    , _randSeed   :: StdGen
    , _delays     :: Delays 
    }

$(makeLenses ''Emulation)

instance Default Delays where
    def = Delays . const . const . return $ 0

instance Default Emulation where
    def = emulation (mkStdGen def) def

emulation :: StdGen -> Delays -> Emulation
emulation = Emulation def def def 

addEvent :: Timestamp -> (Addr, Event) -> Emulation -> Emulation
addEvent time event = events.at time ?~ event 

addDelayedEvent :: Timestamp -> (Addr, Event) -> Emulation -> Emulation
addDelayedEvent time event emu = 
    let delayR = uncurry (evalDelay $ emu ^. delays) event
        delayM = evalRandT delayR $ emu ^. randSeed
    in  maybe emu (\delay -> addEvent (time + delay) event emu) delayM

-- that scares me
-- TODO: make like in MessagePack 
call :: Addr 
     -> Addr
     -> Timestamp
     -> FunCoord
     -> Arguments
     -> Emulation 
     -> (Emulation -> Result -> IO ()) 
     -> IO Emulation
call saddr daddr t fun args emu handleResp = 
    return $ addDelayedEvent t (daddr, event) newEmu
  where
    event = RpcRec 
        { sender   = saddr
        , rpcId    = emu ^. rpcCounter
        , funCoord = fun
        , args     = args
        }
    newEmu = emu & rpcCounter +~ 1
    

start :: Emulation -> IO ()
start = void . runMaybeT . loop
  where
    loop :: Emulation -> MaybeT IO Emulation
    loop emu = do
        (time, event) <- MaybeT . return $ Map.lookupGT 0 (emu ^. events)
        let emu1 = emu & events %~ Map.delete time 
        emu2 <- lift $ handleEvent event emu1 
        loop emu2

    handleEvent :: (Addr, Event) -> Emulation -> IO Emulation
    handleEvent (addr, Invoke f)   emu = f emu >> return emu
    handleEvent (addr, RpcRec{..}) emu = undefined 
    handleEvent (addr, RpcAns{..}) emu = undefined

