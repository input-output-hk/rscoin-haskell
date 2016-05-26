-- | Structure and functions used to print transactions.

module GUI.RSCoin.ExplicitTransaction
    ( ExplicitTransaction (..)
    , fromTransaction
    , getTransactionAmount
    ) where

import           Control.Monad  (forM)
import           Data.Bifunctor (first)
import           Data.Int       (Int64)

import           RSCoin.Core    (Address (..), Coin (..), Transaction (..),
                                 getTransactionById)
import           RSCoin.Timed   (WorkMode)

-- | Transaction in a user-printable form.
data ExplicitTransaction = ExplicitTransaction
    { vtInputs  :: [(Maybe Address, Coin)]
    , vtOutputs :: [(Address, Coin)]
    }

-- | Transforms a transaction into a user-printable form.
fromTransaction :: WorkMode m => Transaction -> m ExplicitTransaction
fromTransaction (Transaction i o) = do
    ti <- forM i $ \(tId, x, c) -> do
        pt <- getTransactionById tId
        let ua = (\a -> fst (txOutputs a !! x)) <$> pt
        return (ua, c)
    return $ ExplicitTransaction ti o

-- | Calculates the balance change for the user caused by the transaction.
getTransactionAmount :: [Address] -> ExplicitTransaction -> Int64
getTransactionAmount addrs (ExplicitTransaction i o) =
    calculate (map (first Just) o) - calculate i
  where
    calculate [] = 0
    calculate (x:xs) = calculate xs + (if isMy x then getCoin $ snd x else 0)

    isMy (Nothing, _) = False
    isMy (Just d, _) = getAddress d `elem` map getAddress addrs
