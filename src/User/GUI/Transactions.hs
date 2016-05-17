-- TODO make a description

module GUI.Transactions
    ( VerboseTransaction (..)
    , fromTransaction
    , getTransactionAmount
    , showTransaction
    ) where

import           Control.Lens       ((^.))
import           Control.Monad      (forM)
import           Data.Bifunctor     (first)
import           Data.Int           (Int64)

import           RSCoin.Core        (Address (..), Coin (..), Transaction (..),
                                     getTransactionById, printPublicKey)
import           RSCoin.Timed       (WorkMode)
import           RSCoin.User.Wallet (UserAddress, publicAddress)

data VerboseTransaction = VT
    { vtInputs  :: [(Maybe Address, Coin)]
    , vtOutputs :: [(Address, Coin)]
    }

fromTransaction :: WorkMode m => Transaction -> m VerboseTransaction
fromTransaction (Transaction i o) = do
    ti <- forM i $ \(tId, x, c) -> do
        pt <- getTransactionById tId
        let ua = (\a -> fst (txOutputs a !! x)) <$> pt
        return (ua, c)
    return $ VT ti o

getTransactionAmount :: [UserAddress] -> VerboseTransaction -> Int64
getTransactionAmount a (VT i o) =
    calculate (map (first Just) o) - calculate i
  where
    calculate [] = 0
    calculate (x:xs) = calculate xs + (if isMy x then getCoin $ snd x else 0)

    isMy (Nothing, _) = False
    isMy (Just d, _) = getAddress d `elem` map (^. publicAddress) a

showTransaction :: VerboseTransaction -> String
showTransaction (VT i o) = "[" ++ show1 i ++ " -> " ++
    show1 (map (first Just) o)
  where
    show1 [] = "]"
    show1 [x] = show2 x ++ "]"
    show1 (x:xs) = show2 x ++ ", " ++ show1 xs

    show2 (Nothing, Coin c) = "(Unrecognized, " ++ show c ++ ")"
    show2 (Just (Address a), Coin c) = "(" ++ show3 a ++ ", " ++ show c ++ ")"

    show3 k = drop 8 $ init $ printPublicKey k
