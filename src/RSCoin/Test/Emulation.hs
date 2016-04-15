{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module contains pure implementation of MonadTimed and RpcMonad.
--   Third layer is coming - AcidState

module RSCoin.Test.Emulation 
       ( 
       ) where

import          Data.Map                    as Map
import          Control.Applicative         ((<$>))
import          Control.Monad               (void)
import          Control.Monad.Random        (RandT, evalRandT)
import          Control.Monad.State
import          Control.Monad.Trans         (lift, liftIO)
import          Control.Monad.Trans.Maybe   (MaybeT(..), runMaybeT)
import          Data.Default                (Default, def)
import          System.Random               (StdGen, mkStdGen)
import          Control.Lens                ((+~), (&), (%~), (?~), (^.), 
                                             makeLenses, at)
import          Data.MessagePack.Object     (Object(..), MessagePack, toObject, fromObject)

import          RSCoin.Test.MonadTimed
import          RSCoin.Test.RpcMonad

------------ There is no anything interesting in this module for now -----------------

-- | Something measured in microseconds
type Mcs = Int

type Timestamp = Mcs

type FunCoord = (Addr, Func)

-- TODO: replace with actual arguments, make safe functions e.t.c.
type Arguments = Int
type Result = Int

-- |Function name and arguments to pass into it
type Func = (String, [Object])

-- like RpcType from MessagePack
-- it's implementation is hiden, so need our own one
class Args r where
    rpcc :: [Object] -> r

instance Args [Object] where
    rpcc = id

instance (MessagePack o, Args r) => Args (o -> r) where
    rpcc o os = rpcc $ [toObject o]  -- TODO: reverse and use (:)


data Event 
    = Invoke (() -> IO ())
    | RpcRec    
        { sender   :: Addr
        , rpcId    :: Int
        , funCoord :: FunCoord
        }
    | RpcAns
        { rpcId    :: Int 
        , answer   :: Result
        }

newtype Delays = Delays 
    { -- | Just delay if net packet delivered successfully
      --   Nothing otherwise
      evalDelay :: Addr -> Event -> RandT StdGen Maybe Mcs
      -- ^ I still think that this function is at right place
      --   We just need to find funny syntax for creating complex description
      --   of network nastinesses.
      --   Maybe like this one:
      {- 
        delays $ do  
                       during (10, 20) .= Probabitiy 60
            requests . before 30       .= Delay (5, 7)
            for "mintette2" $ do
                during (40, 150)       .= Probability 30 <> DelayUpTo 4
                responses . after 200  .= Disabled     
      -}
      --   First what came to mind.
      --   Or maybe someone has overall better solution in mind
    }

-- state for MonadTimed
data Timing = Timing 
    { --TODO: need priority queue here (or at least multimap)
      _events  :: Map.Map Timestamp (Addr, Event) 
    , _curTime :: MicroSeconds
    }
$(makeLenses ''Timing)

data NetInfo = NetInfo 
    {_listeners  :: Map.Map FunCoord (Arguments -> Result)
    , _randSeed   :: StdGen
    , _delays     :: Delays 
    }
$(makeLenses ''NetInfo)

-- state for RpcMonad
-- TODO: how to name it normally?
data RpcInfo = RpcInfo
    { _netInfo   :: NetInfo
    , _localAddr :: Addr
    }
$(makeLenses ''RpcInfo)

instance Default Delays where
    -- | Descirbes reliable network
    def  =  Delays . const . const . return $ 0

-- instance Default Emulation where
--    def = emulation (mkStdGen def) def

-- emulation :: StdGen -> Delays -> Emulation
-- emulation = Emulation def def def 

-- | Actual Emulation. Implements MonadTimed and RpcMonad
--   @martoon: wowow, I've never thought I'd use StateT inside StateT, 
--   but, as abstractions are used, I guess it's ok
type Emulation a  =  StateT RpcInfo (StateT Timing IO) a


-- implementations 

instance Monad m => MonadTimed (StateT Timing m) where
    schedule timeMod action = undefined

    -- need Cont monad here
    invoke timeMod action = undefined

{- 
addEvent :: Timestamp -> (Addr, Event) -> Emulation -> Emulation
addEvent time event = events.at time ?~ event 

addDelayedEvent :: Timestamp -> (Addr, Event) -> Emulation -> Emulation
addDelayedEvent time event emu = 
    let delayR = uncurry (evalDelay $ emu ^. delays) event
        delayM = evalRandT delayR $ emu ^. randSeed
    in  maybe emu (\delay -> addEvent (time + delay) event emu) delayM
-}

