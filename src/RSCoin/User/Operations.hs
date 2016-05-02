{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Basic operations with wallet.

module RSCoin.User.Operations
       ( commitError
       , getAmount
       , updateToBlockHeight
       , formTransaction
       ) where

import           Control.Lens          ((^.))
import           Control.Monad         (filterM, unless, when)
import           Control.Monad.Catch   (MonadThrow, throwM)
import           Control.Monad.Trans   (liftIO)
import           Data.Acid             (query, update)
import           Data.Int              (Int64)
import           Data.Function         (on)
import           Data.List             (nub, nubBy)
import qualified Data.Map              as M
import           Data.Maybe            (isJust)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Tuple.Select     (sel1)

import           Serokell.Util.Text    (format', formatSingle')

import           RSCoin.Core           as C
import           RSCoin.Test           (WorkMode)
import           RSCoin.User.AcidState (GetAllAddresses (..))
import qualified RSCoin.User.AcidState as A
import           RSCoin.User.Error     (UserError (..))
import           RSCoin.User.Logic     (validateTransaction)
import qualified RSCoin.User.Wallet    as W

commitError :: MonadThrow m => T.Text -> m ()
commitError = throwM . InputProcessingError

getAmount :: A.RSCoinUserState -> W.UserAddress -> IO C.Coin
getAmount st userAddress =
    sum . map getValue <$> query st (A.GetTransactions userAddress)
  where
    getValue =
        C.getAmountByAddress $ W.toAddress userAddress

-- | Updates wallet to given blockchain height assuming that it's in
-- previous height state already.
updateToBlockHeight :: WorkMode m => A.RSCoinUserState -> PeriodId -> m ()
updateToBlockHeight st newHeight = do
    C.HBlock{..} <- getBlockByHeight newHeight
    -- TODO validate this block with integrity check that we don't have
    liftIO $ update st $ A.WithBlockchainUpdate newHeight hbTransactions

-- | Forms transaction out of user input and sends it to the net.
formTransaction :: WorkMode m => 
    A.RSCoinUserState -> [(Int, Int64)] -> Address -> Coin -> m ()
formTransaction st inputs outputAddr outputCoin = do
    when
        (nubBy ((==) `on` fst) inputs /= inputs) $
        commitError "All input addresses should have distinct IDs."
    unless (all (> 0) $ map snd inputs) $
        commitError $
        formatSingle'
            "All input values should be positive, but encountered {}, that's not." $
        head $ filter (<= 0) $ map snd inputs
    accounts <- liftIO $ query st GetAllAddresses
    let notInRange i = i <= 0 || i > length accounts
    when
        (any notInRange $ map fst inputs) $
        commitError $
        format'
            "Found an account id ({}) that's not in [1..{}]"
            ( head $ filter notInRange $ map fst inputs
            , length accounts)
    let accInputs :: [(Int, W.UserAddress, Coin)]
        accInputs = map (\(i,c) -> (i, accounts !! (i - 1), C.Coin c)) inputs
        hasEnoughFunds (i,acc,c) = liftIO $ do
            amount <- getAmount st acc
            return $
                if amount >= c
                    then Nothing
                    else Just i
    overSpentAccounts <-
        filterM (\a -> isJust <$> hasEnoughFunds a) accInputs
    unless (null overSpentAccounts) $
        commitError $
        (if length overSpentAccounts > 1
             then "At least the"
             else "The") <>
        formatSingle'
            " following account doesn't have enough coins: {}"
            (sel1 $ head overSpentAccounts)
    (addrPairList,outTr) <- liftIO $
        foldl1 mergeTransactions <$> mapM formTransactionMapper accInputs
    liftIO $ TIO.putStrLn $ 
        formatSingle' "Please check your transaction: {}" outTr
    walletHeight <- liftIO $ query st A.GetLastBlockId
    lastBlockHeight <- pred <$> getBlockchainHeight
    when (walletHeight /= lastBlockHeight) $
        commitError $
        format'
            ("Wallet isn't updated (lastBlockHeight {} when blockchain's last block is {}). " <>
             "Please synchonize it with blockchain. The transaction wouldn't be sent.")
            (walletHeight, lastBlockHeight)
    let signatures =
            M.fromList $
            map (\(addrid',address') ->
                      (addrid', sign (address' ^. W.privateAddress) outTr))
                addrPairList
    validateTransaction outTr signatures $ lastBlockHeight + 1
    liftIO $ update st $ A.AddTemporaryTransaction outTr
  where
    formTransactionMapper :: (Int, W.UserAddress, Coin)
                          -> IO ([(AddrId, W.UserAddress)], Transaction)
    formTransactionMapper (_,a,c) = do
        (addrids :: [C.AddrId]) <-
            concatMap (getAddrIdByAddress $ W.toAddress a) <$>
            query st (A.GetTransactions a)
        let (chosen,leftCoin) = chooseAddresses addrids c
            transaction =
                Transaction chosen $
                (outputAddr, outputCoin) :
                (if leftCoin == 0
                     then []
                     else [(W.toAddress a, leftCoin)])
            addrPairList = map (, a) chosen
        return (addrPairList, transaction)
    mergeTransactions
        :: ([(AddrId, W.UserAddress)], Transaction)
        -> ([(AddrId, W.UserAddress)], Transaction)
        -> ([(AddrId, W.UserAddress)], Transaction)
    mergeTransactions (s1,a) (s2,b) =
        ( nub $ s1 <> s2
        , Transaction
              (txInputs a <> txInputs b)
              (nub $ txOutputs a <> txOutputs b))
