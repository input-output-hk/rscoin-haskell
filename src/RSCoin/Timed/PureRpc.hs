{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module RSCoin.Timed.PureRpc
    ( PureRpc
    , runPureRpc
    , runPureRpc_
    , localPlatformLayout
    , Delays(..)
    ) where

import           Control.Lens            (makeLenses, use, (%%=), (%=))
import           Control.Monad           (forM_)
import           Control.Monad.Catch     (MonadCatch, MonadMask, MonadThrow,
                                          throwM)
import           Control.Monad.Random    (Rand, runRand)
import           Control.Monad.State     (MonadState (get, put, state), StateT,
                                          evalStateT, get, put)
import           Control.Monad.Trans     (MonadIO, MonadTrans, lift)
import           Data.Default            (Default, def)
import           Data.Map                as Map
import           Data.String             (IsString)
import           System.Random           (StdGen)

import           Data.MessagePack        (Object)
import           Data.MessagePack.Object (MessagePack, fromObject, toObject)

import           RSCoin.Timed.MonadRpc   (Addr, Client (..), Host, Method (..), MonadRpc (execClient, getPlatformLayout, serve),
                                          PlatformLayout (..), RpcError (..),
                                          methodBody, methodName)
import           RSCoin.Timed.MonadTimed (Microsecond, MonadTimed, for,
                                          localTime, mcs, minute, wait)
import           RSCoin.Timed.Timed      (TimedT, evalTimedT, runTimedT)

-- | List of known issues:
--     -) Method, once being declared in net, can't be removed
--        Even timeout won't help
--        Status: not relevant in tests for now
--     -) Connection can't be refused, only be held on much time
--        Status: not relevant until used with fixed timeout

data RpcStage = Request | Response

localhost :: IsString s => s
localhost = "127.0.0.1"

localPlatformLayout :: PlatformLayout
localPlatformLayout = PlatformLayout (localhost, 3000) (localhost, 3001)

-- @TODO Remove these hard-coded values
-- | Describes network nastyness
newtype Delays = Delays
    { -- | Just delay if net packet delivered successfully
      --   Nothing otherwise
      -- TODO: more parameters
      evalDelay :: RpcStage -> Microsecond -> Rand StdGen (Maybe Microsecond)
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

-- | Keeps servers' methods
type Listeners m = Map.Map (Addr, String) ([Object] -> m Object)

-- | Keeps global network information
data NetInfo m = NetInfo
    { _listeners :: Listeners m
    , _randSeed  :: StdGen
    , _delays    :: Delays
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

-- | Launches rpc scenario.
runPureRpc
    :: (MonadIO m, MonadCatch m)
    => StdGen -> Delays -> PureRpc m a -> m a
runPureRpc _randSeed _delays (PureRpc rpc) =
    evalStateT (evalTimedT (evalStateT rpc localhost)) net
  where
    net        = NetInfo{..}
    _listeners = Map.empty

-- | Launches rpc scenario without result. May be slightly more efficient.
runPureRpc_
    :: (MonadIO m, MonadCatch m)
    => StdGen -> Delays -> PureRpc m () -> m ()
runPureRpc_ _randSeed _delays (PureRpc rpc) =
    evalStateT (runTimedT (evalStateT rpc localhost)) net
  where
    net        = NetInfo{..}
    _listeners = Map.empty

-- TODO: use normal exceptions here
request :: (Monad m, MonadThrow m, MessagePack a)
        => Client a
        -> (Listeners (PureRpc m), Addr)
        -> PureRpc m a
request (Client name args) (listeners', addr) =
    case Map.lookup (addr, name) listeners' of
        Nothing -> throwM $ ServerError $ toObject $ mconcat
            ["method \"", name, "\" not found at adress ", show addr]
        Just f  -> do
            res <- f args
            case fromObject res of
                Nothing -> throwM $ ResultTypeError "type mismatch"
                Just r  -> return r


instance (MonadIO m, MonadThrow m, MonadCatch m) => MonadRpc (PureRpc m) where
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
        sleepForever
      where
        sleepForever = wait (for 100500 minute) >> sleepForever

    -- @TODO not sure it's ok when it comes to signers
    getPlatformLayout = pure $ localPlatformLayout

waitDelay :: (MonadThrow m, MonadIO m, MonadCatch m) => RpcStage -> PureRpc m ()
waitDelay stage =
    PureRpc $
    do delays' <- lift . lift $ use delays
       time <- localTime
       delay <- lift . lift $ randSeed %%= runRand (evalDelay delays' stage time)
       wait $ maybe (for 99999 minute) (`for` mcs) delay
