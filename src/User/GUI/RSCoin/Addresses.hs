module GUI.RSCoin.Addresses
    ( VerboseAddress (..)
    , getAddresses
    ) where

import           Control.Lens       ((^.))
import           Control.Monad      (forM)
import qualified Data.IntMap.Strict as M

import           RSCoin.Core        (Coin (..), CoinAmount, PublicKey,
                                     defaultNodeContext, genesisAddress,
                                     getAddress, userLoggerName)
import           RSCoin.Timed       (ContextArgument (..), runRealModeUntrusted)
import           RSCoin.User        (GetOwnedDefaultAddresses (..), UserState,
                                     getAmountNoUpdate, query)

data VerboseAddress = VA
    { address :: PublicKey
    , balance :: CoinAmount
    }

-- FIXME: this is used only in gui. Now that we are using Rational in
-- Coin I am not sure what is correct way to implement this. For now I
-- will just round the value.
getAddresses :: Maybe FilePath -> UserState -> IO [VerboseAddress]
getAddresses confPath st = do
    as <-
        query st $
        GetOwnedDefaultAddresses $ defaultNodeContext ^. genesisAddress
    let ctxArg = maybe CADefaultLocation CACustomLocation confPath
    forM as $
        \a ->
             runRealModeUntrusted userLoggerName ctxArg $
             do b <- M.findWithDefault 0 0 <$> getAmountNoUpdate st a
                return $ VA (getAddress a) (getCoin b)
