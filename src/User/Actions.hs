{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Actions (proceedCommand) where

import           Control.Lens       ((^.))
import           Data.Acid          (query)
import qualified Data.Text.IO       as TIO

import           Serokell.Util.Text (format')

import           AcidState          (GetAllAddresses (..))
import qualified AcidState          as A
import           RSCoin.Core        as C
import qualified UserOptions        as O
import qualified Wallet             as W

getAmount :: A.RSCoinUserState -> W.UserAddress -> IO C.Coin
getAmount st userAddress =
    sum . map getValue <$> query st (A.GetTransactions userAddress)
  where
    getValue =
        C.getAmountByAddress $ C.Address $ userAddress ^. W.publicAddress

proceedCommand :: A.RSCoinUserState -> O.UserCommand -> IO ()
proceedCommand st O.ListWallets = do
    (wallets :: [(C.PublicKey,C.Coin)]) <-
        mapM (\w -> (w ^. W.publicAddress,) <$> getAmount st w) =<<
        query st GetAllAddresses
    TIO.putStrLn "Here's the list of transactions:"
    TIO.putStrLn "Num | Public ID"
    mapM_ (TIO.putStrLn . format' "{}. {} : {}") $
        uncurry (zip3 [(1 :: Integer) ..]) $ unzip wallets
proceedCommand _ _ = putStrLn "Not implemented."
