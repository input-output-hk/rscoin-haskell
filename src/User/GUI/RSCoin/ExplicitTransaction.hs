{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Structure and functions used to print transactions.

module GUI.RSCoin.ExplicitTransaction
    ( ExplicitTransaction (..)
    , fromTransaction
    , getTransactionAmount
    ) where

import           Control.Exception      (SomeException)
import           Control.Monad          (forM)
import           Control.Monad.Catch    (catch)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bifunctor         (first)

import           GUI.RSCoin.GUIAcid     (GUIState, addTransaction,
                                         getTransaction)
import           RSCoin.Core            (Address (..), Coin (..),
                                         CoinAmount (..), Transaction (..),
                                         getTransactionById)
import           RSCoin.Timed           (WorkMode)

-- | Transaction in a user-printable form.
data ExplicitTransaction = ExplicitTransaction
    { vtInputs  :: [(Maybe Address, Coin)]
    , vtOutputs :: [(Address, Coin)]
    }

-- | Transforms a transaction into a user-printable form.
fromTransaction
    :: WorkMode m => GUIState -> Transaction -> m ExplicitTransaction
fromTransaction st (Transaction i o) = do
    ti <- forM i $ \(tId, x, c) -> do
        pt <- getTransactionWithDB tId
        let ua = (\a -> fst (txOutputs a !! x)) <$> pt
        return (ua, c)
    return $ ExplicitTransaction ti o
  where
    performRequest tId = do
        pt <- getTransactionById tId
        liftIO $ addTransaction st tId pt
        return pt
    getTransactionWithDB tId = do
        mt <- liftIO $ getTransaction st tId
        case mt of
            Nothing -> performRequest tId
                           `catch`
                           (\(_ :: SomeException) -> return Nothing)
            Just t -> return t

-- | Calculates the balance change for the user caused by the transaction.
getTransactionAmount :: [Address] -> ExplicitTransaction -> CoinAmount
getTransactionAmount addrs (ExplicitTransaction i o) =
    calculate (map (first Just) o) - calculate i
  where
    zeroCoin = CoinAmount 0

    calculate [] = zeroCoin
    calculate (x:xs) = calculate xs + (if isMy x then getCoin $ snd x else zeroCoin)

    isMy (Nothing, _) = False
    isMy (Just d, _) = getAddress d `elem` map getAddress addrs
