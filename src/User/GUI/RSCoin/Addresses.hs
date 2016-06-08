module GUI.RSCoin.Addresses
    ( VerboseAddress (..)
    , getAddresses
    ) where

import           Control.Lens  ((^.))
import           Control.Monad (forM)
import           Data.Acid     (query)
import           Data.Int      (Int64)

import           RSCoin.Core   (Coin (..), PublicKey)
import           RSCoin.Timed  (runRealModeLocal)
import           RSCoin.User   (GetAllAddresses (..), RSCoinUserState,
                                getAmountNoUpdate, publicAddress)

data VerboseAddress = VA
    { address :: PublicKey
    , balance :: Int64
    }

getAddresses :: RSCoinUserState -> IO [VerboseAddress]
getAddresses st = do
    as <- query st GetAllAddresses
    forM as $ \a -> runRealModeLocal $ do
        b <- getAmountNoUpdate st a
        return $ VA (a ^. publicAddress) (getCoin b)
