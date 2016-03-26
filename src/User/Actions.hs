{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Actions (proceedCommand) where

import           Control.Exception  (throwIO)
import           Control.Lens       ((^.))
import           Control.Monad      (filterM, unless, when)
import           Data.Acid          (query)
import           Data.Int           (Int64)
import           Data.List          (nubBy)
import           Data.Maybe         (isJust)
import           Data.Monoid        ((<>))
import           Data.Ord           (comparing)
import qualified Data.Text.IO       as TIO
import           Data.Tuple.Select  (sel1)

import           Serokell.Util.Text (format', formatSingle')

import           AcidState          (GetAllAddresses (..))
import qualified AcidState          as A
import           RSCoin.Core        as C
import           UserError          (UserError (..), eWrap)
import qualified UserOptions        as O
import qualified Wallet             as W

getAmount :: A.RSCoinUserState -> W.UserAddress -> IO C.Coin
getAmount st userAddress =
    sum . map getValue <$> query st (A.GetTransactions userAddress)
  where
    getValue =
        C.getAmountByAddress $ W.userAddressToAddress userAddress

-- | Given the state of program and command, makes correspondent
-- actions.
proceedCommand :: A.RSCoinUserState -> O.UserCommand -> IO ()
proceedCommand st O.ListAddresses =
    eWrap $
    do (wallets :: [(C.PublicKey, C.Coin)]) <-
           mapM
               (\w ->
                     (w ^. W.publicAddress, ) <$> getAmount st w) =<<
           query st GetAllAddresses
       TIO.putStrLn "Here's the list of transactions:"
       TIO.putStrLn "Num | Public ID | Amount"
       mapM_ (TIO.putStrLn . format' "{}. {} : {}") $
           uncurry (zip3 [(1 :: Integer) ..]) $ unzip wallets
proceedCommand st (O.FormTransaction inputs (outputAddrStr,outputCoinInt)) =
    eWrap $
    formTransaction st inputs (undefined outputAddrStr) $ C.Coin outputCoinInt
proceedCommand _ _ = putStrLn "Not implemented."

formTransaction :: A.RSCoinUserState -> [(Int, Int64)] -> Address -> Coin -> IO ()
formTransaction st inputs outputAddr outputCoin =
    do when (nubBy (\a -> (== EQ) . comparing fst a) inputs /= inputs) $
           commitError "All input addresses should have distinct IDs."
       unless (all (> 0) $ map snd inputs) $
           commitError $
           formatSingle'
               "All input values should be positive, but encountered {}, that's not." $
           head $ filter (<= 0) $ map snd inputs
       accounts <- query st GetAllAddresses
       when (any (>= length accounts) $ map fst inputs) $
           commitError $
           format'
               "Found an account id ({}) greater than total number of accounts in wallet ({})"
               ( head $ filter (>= length accounts) $ map fst inputs
               , length accounts)
       let accInputs :: [(Int, W.UserAddress, Coin)]
           accInputs = map (\(i,c) -> (i, accounts !! (i - 1), C.Coin c)) inputs
           hasEnoughFunds (i,acc,c) = do
               amount <- getAmount st acc
               return $ if amount >= c
                        then Nothing
                        else Just i
       overSpentAccounts <- filterM (\a -> isJust <$> hasEnoughFunds a) accInputs
       unless (null overSpentAccounts) $
           commitError $
           (if length overSpentAccounts > 1
                then "At least the"
                else "The") <>
           formatSingle' " following account doesn't have enough coins: {}"
                         (sel1 $ head overSpentAccounts)
       outTr <- mconcat <$> mapM formTransactionMapper accInputs
       TIO.putStrLn $ formatSingle' "Please check your transaction: {}" outTr
       -- Here should be call to push this transaction to mintettes
  where
    formTransactionMapper :: (Int, W.UserAddress, Coin) -> IO Transaction
    formTransactionMapper (_, a, c) = do
        (addrids :: [C.AddrId]) <-
            concatMap (getAddrIdByAddress $ W.userAddressToAddress a) <$>
                query st (A.GetTransactions a)
        let (chosen, leftCoin) =
                chooseAddresses addrids c
        return $ Transaction chosen $
            (outputAddr, outputCoin) :
                (if leftCoin == 0
                 then []
                 else [(W.userAddressToAddress a, leftCoin)])
    commitError = throwIO . InputProcessingError
