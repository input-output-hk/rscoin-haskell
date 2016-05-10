{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module RSCoin.Timed.PureRpc
    ( PureRpc
    , runPureRpc
    , Delays(..)
    ) where

import           Control.Lens                (use, makeLenses, (.=), (%=))
import           Control.Monad               (forM_)
import           Control.Monad.Catch         (MonadThrow, MonadCatch, MonadMask)
import           Control.Monad.State         (StateT, put, evalStateT, get,
                                              MonadState (state, get, put))
import           Control.Monad.Trans         (lift, MonadIO, MonadTrans)
import           Control.Monad.Random        (Rand, runRand)
import           Data.Default                (Default, def)
import           Data.Maybe                  (fromMaybe)
import           Data.Map                    as Map
import           System.Random               (StdGen)

import           Data.MessagePack            (Object)
import           Data.MessagePack.Object     (fromObject, MessagePack)

import           RSCoin.Timed.MonadTimed      (MonadTimed, MicroSeconds, for,
                                              wait, sec, mcs, localTime)
import           RSCoin.Timed.MonadRpc        (MonadRpc, execClient, serve,
                                              Addr, Method(..), Client(..),
                                              methodName, methodBody, Host)
import           RSCoin.Timed.Timed           (runTimedT, TimedT)


-- | Describes network nastyness
newtype Delays = Delays 
    { -- | Just delay if net packet delivered successfully
      --   Nothing otherwise
      -- TODO: more parameters
      -- FIXME: we should handle StdGen with Quickcheck.Arbitrary
      evalDelay :: RpcStage -> MicroSeconds -> Rand StdGen (Maybe MicroSeconds)
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

instance Default Delays where
    -- | Descirbes reliable network
    def = Delays . const . const . return . Just $ 0

data RpcStage  =  Request
               |  Response

-- | Keeps servers' methods
type Listeners m  =  Map.Map (Addr, String) ([Object] -> m Object)

-- | Keeps global network information
data NetInfo m = NetInfo 
    { _listeners  :: Listeners m
    , _randSeed   :: StdGen
    , _delays     :: Delays 
    }
$(makeLenses ''NetInfo)

-- | Pure implementation of RPC
newtype PureRpc m a = PureRpc 
    { unwrapPureRpc :: StateT Host (TimedT (StateT (NetInfo (PureRpc m)) m)) a 
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTimed
               , MonadThrow, MonadCatch, MonadMask)

instance MonadTrans PureRpc where
    lift = PureRpc . lift . lift . lift

instance MonadState s m => MonadState s (PureRpc m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | Launches rpc scenario
runPureRpc :: (Monad m, MonadCatch m) => StdGen -> Delays -> PureRpc m () -> m ()
runPureRpc _randSeed _delays (PureRpc rpc) = do
    evalStateT (runTimedT (evalStateT rpc "127.0.0.1")) net
  where
    net        = NetInfo{..}
    _listeners = Map.empty

-- TODO: use normal exceptions here
request :: Monad m => MessagePack a => Client a -> (Listeners (PureRpc m), Addr) -> PureRpc m a
request (Client name args) (listeners', addr) = do
    case Map.lookup (addr, name) listeners' of
        Nothing -> error $ mconcat 
            ["Method ", name, " is not defined at ", show addr]
        Just f  -> fromMaybe (error "Answer type mismatch")
                 . fromObject <$> f args

instance (Monad m, MonadThrow m) => MonadRpc (PureRpc m) where
    execClient addr cli = PureRpc $ do  
        curHost <- get
        unwrapPureRpc $ waitDelay Request

        ls <- lift . lift $ use listeners
        put $ fst addr
        answer <- unwrapPureRpc $ request cli (ls, addr)
        unwrapPureRpc $ waitDelay Response
        
        put curHost
        return answer
    
    serve port methods = PureRpc $ do
        host <- get
        lift $ lift $ forM_ methods $ \Method{..} -> 
            listeners %= Map.insert ((host, port), methodName) methodBody

waitDelay :: MonadThrow m => RpcStage -> PureRpc m () 
waitDelay stage  =  PureRpc $ do
    seed    <- lift . lift $ use randSeed
    delays' <- lift . lift $ use delays
    time    <- localTime
    let (delay, nextSeed) = runRand (evalDelay delays' stage time) seed 
    lift $ lift $ randSeed .= nextSeed
    wait $ maybe (for 99999 sec) (\t -> for t mcs) $ delay 
        -- TODO: throw or eliminate
