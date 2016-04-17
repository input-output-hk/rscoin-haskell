module RSCoin.Test.PureRpc
    (
    ) where

import           RSCoin.Test.Timed           (TimedT(..))
import           System.Random               (StdGen, mkStdGen)
import           Control.Monad.Random        (RandT, evalRandT)

newtype Delays = Delays 
    { -- | Just delay if net packet delivered successfully
      --   Nothing otherwise
      evalDelay :: Addr -> () -> RandT StdGen Maybe MicroSeconds
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
    def = Delays . const . const . return $ 0

data NetInfo = NetInfo 
    { _listeners  :: Map.Map FunCoord (Arguments -> Result)
    , _randSeed   :: StdGen
    , _delays     :: Delays 
    }
$(makeLenses ''NetInfo)

type LocalAddr = Addr

newtype PureRpc m a = StateT LocalAddr (TimedT (StateT NetInfo m)) a



{-
addDelayedEvent :: Timestamp -> (Addr, Event) -> Emulation -> Emulation
addDelayedEvent time event emu = 
    let delayR = uncurry (evalDelay $ emu ^. delays) event
        delayM = evalRandT delayR $ emu ^. randSeed
    in  maybe emu (\delay -> addEvent (time + delay) event emu) delayM
-}

