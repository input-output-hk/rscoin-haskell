module GUI.RSCoin.Addresses
    ( VerboseAddress (..)
    , getAddresses
    ) where

import           Control.Monad           (forM)
import           Data.Acid               (query)
import qualified Data.IntMap.Strict      as M

import           RSCoin.Core             (Coin (..), CoinAmount, PublicKey,
                                          defaultNodeContext, getAddress)
import           RSCoin.Timed            (runRealModeUntrusted)
import           RSCoin.User             (GetOwnedDefaultAddresses (..),
                                          RSCoinUserState,
                                          getAmountNoUpdate)

data VerboseAddress = VA
    { address :: PublicKey
    , balance :: CoinAmount
    }

-- FIXME: this is used only in gui. Now that we are using Rational in
-- Coin I am not sure what is correct way to implement this. For now I
-- will just round the value.
getAddresses :: Maybe FilePath -> RSCoinUserState -> IO [VerboseAddress]
getAddresses confPath st = do
    as <- query st $ GetOwnedDefaultAddresses defaultNodeContext
    forM as $ \a -> runRealModeUntrusted confPath $ do
        b <- M.findWithDefault 0 0 <$> getAmountNoUpdate st a
        return $ VA (getAddress a) (getCoin b)
