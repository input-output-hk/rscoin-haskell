module GUI.RSCoin.Addresses
    ( VerboseAddress (..)
    , getAddresses
    ) where

import           Control.Monad (forM)
import           Data.Acid     (query)
import qualified Data.Map      as M

import           RSCoin.Core   (Coin (..), PublicKey, getAddress)
import           RSCoin.Timed  (runRealModeLocal)
import           RSCoin.User   (GetPublicAddresses (..), RSCoinUserState,
                                getAmountNoUpdate)

data VerboseAddress = VA
    { address :: PublicKey
    , balance :: Rational
    }

-- FIXME: this is used only in gui. Now that we are using Rational in
-- Coin I am not sure what is correct way to implement this. For now I
-- will just round the value.
getAddresses :: RSCoinUserState -> IO [VerboseAddress]
getAddresses st = do
    as <- query st GetPublicAddresses
    forM as $ \a -> runRealModeLocal $ do
        b <- M.findWithDefault 0 0 <$> getAmountNoUpdate st a
        return $ VA (getAddress a) (getCoin b)
