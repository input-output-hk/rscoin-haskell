{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module RSCoin.Test.PureRpc
    ( PureRpc
    , runPureRpc
    , Delays(..)
    ) where

import           Control.Monad.State         
import           Control.Monad.Reader        (ReaderT, runReaderT, ask)         
import           RSCoin.Test.Timed           (TimedT(..))
import           System.Random               (StdGen, mkStdGen)
import           Control.Monad.Random        (Rand, runRand)
import           Data.Default                (Default, def)
import           Data.Map                    as Map
import           Control.Lens
import           Data.Maybe                  (fromMaybe)
import           Data.MessagePack            (Object)
import           Data.ByteString             (ByteString)

import           Data.MessagePack.Object     (fromObject, MessagePack)

import           RSCoin.Test.MonadTimed      (MonadTimed, MicroSeconds, for
                                             , wait, sec, mcs, localTime)
import           RSCoin.Test.MonadRpc        (MonadRpc, execClient, serve
                                             , Addr, Method(..), Client(..)
                                             , methodName, methodBody, Host)
import           RSCoin.Test.Timed           (TimedT, runTimedT)


newtype Delays = Delays 
    { -- | Just delay if net packet delivered successfully
      --   Nothing otherwise
      -- TODO: more parameters
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

type FunName  =  String

type Listeners m  =  Map.Map (Addr, FunName) ([Object] -> m Object)

-- | Keeps global network information
data NetInfo m = NetInfo 
    { _listeners  :: Listeners m
    , _randSeed   :: StdGen
    , _delays     :: Delays 
    }
$(makeLenses ''NetInfo)

-- | Pure implementation of RPC
newtype PureRpc a = PureRpc 
    { unwrapPureRpc :: StateT Host (TimedT (StateT (NetInfo IO) IO)) a 
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTimed)

-- | Launches rpc scenario
runPureRpc :: StdGen -> Delays -> PureRpc () -> IO ()
runPureRpc _randSeed _delays (PureRpc rpc)  =  do
    evalStateT (runTimedT (evalStateT rpc "localhost")) net
  where
    net        = NetInfo{..}
    _listeners = Map.empty

-- TODO: use normal exceptions here
mkPureClient :: MessagePack a => Client a -> ReaderT (Listeners IO, Addr) IO a
mkPureClient (Client name args) =  do
    (listens', addr) <- ask
    case Map.lookup (addr, name) listens' of
        Nothing -> error $ mconcat 
            ["Method ", name, " is not defined at ", show addr]
        Just f  -> lift $ fromMaybe (error "Answer type mismatch")
                 . fromObject <$> f args

instance MonadRpc PureRpc where
    execClient addr cli  =  PureRpc $ do  
        curHost <- get
        unwrapPureRpc $ waitDelay Request

        ls <- lift . lift $ use listeners
        put $ fst addr
        answer    <- liftIO $ runReaderT (mkPureClient cli) (ls, addr)
        unwrapPureRpc $ waitDelay Response
        
        put curHost
        return answer
    
    serve port methods  =  PureRpc $ do
        host <- get
        lift . lift . forM_ methods $ \Method{..} -> listeners 
            %= Map.insert ((host, port), methodName) methodBody

waitDelay :: RpcStage -> PureRpc () 
waitDelay stage  =  PureRpc $ do
    seed    <- lift . lift $ use randSeed
    host    <- get
    delays' <- lift . lift $ use delays
    time    <- localTime
    let (delay, nextSeed) = runRand (evalDelay delays' stage time) seed 
    lift $ lift $ randSeed .= nextSeed
    wait $ maybe (for 99999 sec) (flip for mcs) $ delay 
        -- TODO: throw or eliminate


