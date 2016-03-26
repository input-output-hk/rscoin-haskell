{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Actions (proceedCommand) where

import           Control.Exception  (throwIO)
import           Control.Lens       ((^.))
import           Control.Monad      (filterM, unless, when)
import           Data.Acid          (query)
import           Data.List          (nubBy)
import           Data.Maybe         (isJust)
import           Data.Monoid        ((<>))
import           Data.Ord           (comparing)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO

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
        C.getAmountByAddress $ C.Address $ userAddress ^. W.publicAddress

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
proceedCommand st (O.FormTransaction inputs output) =
    eWrap $
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
       let accInputs = map (\(i,c) -> (i, , C.Coin c) (accounts !! (i - 1))) inputs
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
                         ((\(a,_,_) -> a) $
                          head overSpentAccounts)
       inputsAccs <- mapM (\(_,a,c) -> (a, , c) <$> query st (A.GetTransactions a)) accInputs
       putStrLn "Not implemented"
  where
    commitError = throwIO . InputProcessingError
proceedCommand _ _ = putStrLn "Not implemented."
