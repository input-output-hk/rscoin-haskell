{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Module that provides some functions that transform
-- UserOptions.UserCommand s to IO actions.

module Actions (proceedCommand) where

import           Control.Exception     (throwIO)
import           Control.Lens          ((^.))
import           Control.Monad         (filterM, forM_, unless, when)
import           Data.Acid             (query, update)
import           Data.Int              (Int64)
import           Data.List             (nub, nubBy)
import           Data.Maybe            (fromJust, isJust)
import           Data.Monoid           ((<>))
import           Data.Ord              (comparing)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Tuple.Select     (sel1)

import           Serokell.Util.Text    (format', formatSingle')

import           RSCoin.Core           as C
import           RSCoin.User.AcidState (GetAllAddresses (..))
import qualified RSCoin.User.AcidState as A
import           RSCoin.User.Error     (UserError (..), eWrap)
import           RSCoin.User.Logic     (getBlockByHeight, getBlockchainHeight)
import qualified RSCoin.User.Wallet    as W
import qualified UserOptions           as O

commitError :: T.Text -> IO ()
commitError = throwIO . InputProcessingError

getAmount :: A.RSCoinUserState -> W.UserAddress -> IO C.Coin
getAmount st userAddress =
    sum . map getValue <$> query st (A.GetTransactions userAddress)
  where
    getValue =
        C.getAmountByAddress $ W.toAddress userAddress

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
    do let pubKey = C.Address <$> C.constructPublicKey outputAddrStr
       unless (isJust pubKey) $
           commitError $
           "Provided key can't be exported: " <> outputAddrStr
       formTransaction st inputs (fromJust pubKey) $
           C.Coin outputCoinInt
proceedCommand st O.UpdateBlockchain =
    eWrap $
    do height <- getBlockchainHeight -- request to get blockchain height
       walletHeight <- query st A.GetLastBlockId
       when (walletHeight < height) $
           throwIO $
           StorageError $
           W.InternalError
               "Blockchain height in wallet is greater than in bank. Critical error."
       if height == walletHeight
           then putStrLn "Blockchain is updated already."
           else forM_ [walletHeight, height] (updateToBlockHeight st)

-- | Updates wallet to given blockchain height assuming that it's in
-- previous height state already.
updateToBlockHeight :: A.RSCoinUserState -> Int -> IO ()
updateToBlockHeight st newHeight = do
    C.HBlock{..} <- fromJust <$> getBlockByHeight newHeight -- FIXME: handle maybe
    -- TODO validate this block
    relatedTransactions <-
        nub . concatMap (toTrs hbTransactions) <$> query st GetAllAddresses
    update st $ A.WithBlockchainUpdate newHeight relatedTransactions
  where
    toTrs :: [C.Transaction] -> W.UserAddress -> [C.Transaction]
    toTrs trs userAddr =
        filter (not . null . C.getAddrIdByAddress (W.toAddress userAddr)) trs

-- | Forms transaction out of user input and sends it to the net.
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
            concatMap (getAddrIdByAddress $ W.toAddress a) <$>
                query st (A.GetTransactions a)
        let (chosen, leftCoin) =
                chooseAddresses addrids c
        return $ Transaction chosen $
            (outputAddr, outputCoin) :
                (if leftCoin == 0
                 then []
                 else [(W.toAddress a, leftCoin)])
