{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Basic operations with wallet.

module RSCoin.User.Operations
       ( walletInitialized
       , commitError
       , getUserTotalAmount
       , getAmount
       , getAmountByIndex
       , getAllPublicAddresses
       , getTransactionsHistory
       , updateToBlockHeight
       , updateBlockchain
       , formTransactionFromAll
       , formTransaction
       , formTransactionRetry
       ) where

import           Control.Exception      (SomeException)
import           Control.Lens           ((^.))
import           Control.Monad          (filterM, forM_, unless, void, when)
import           Control.Monad.Catch    (MonadThrow, catch, throwM, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Acid.Advanced     (query', update')
import           Data.Function          (on)
import           Data.List              (genericIndex, genericLength, nub,
                                         nubBy, sortOn)
import qualified Data.Map               as M
import           Data.Maybe             (isJust)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Tuple.Select      (sel1)
import           Safe                   (atMay)

import           Serokell.Util.Text     (format', formatSingle',
                                         listBuilderJSONIndent, pairBuilder)

import qualified RSCoin.Core            as C
import           RSCoin.Timed           (WorkMode, for, mcs, wait)
import           RSCoin.User.AcidState  (GetAllAddresses (..))
import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Error      (UserError (..))
import           RSCoin.User.Logic      (UserLogicError (FailedToCommit),
                                         validateTransaction)
import qualified RSCoin.User.Wallet     as W

walletInitialized :: MonadIO m => A.RSCoinUserState -> m Bool
walletInitialized st = query' st A.IsInitialized

commitError :: (MonadIO m, MonadThrow m) => T.Text -> m ()
commitError e = do
    C.logError C.userLoggerName e
    throwM . InputProcessingError $ e

-- | Updates wallet to given blockchain height assuming that it's in
-- previous height state already.
updateToBlockHeight :: WorkMode m => A.RSCoinUserState -> C.PeriodId -> m ()
updateToBlockHeight st newHeight = do
    C.HBlock{..} <- C.getBlockByHeight newHeight
    -- TODO validate this block with integrity check that we don't have
    update' st $ A.WithBlockchainUpdate newHeight hbTransactions

-- | Updates blockchain to the last height (that was requested in this
-- function call). Returns bool indicating that blockchain was or
-- wasn't updated. Does throw exception when update is not possible
-- due to some error. Thus 'False' return means that blockchain wasn't
-- updated and it's ok (already at max height)
updateBlockchain :: WorkMode m => A.RSCoinUserState -> Bool -> m Bool
updateBlockchain st verbose = do
    walletHeight <- query' st A.GetLastBlockId
    verboseSay $
        formatSingle'
            "Current known blockchain's height (last HBLock's id) is {}."
            walletHeight
    lastBlockHeight <- pred <$> C.getBlockchainHeight
    verboseSay $
        formatSingle'
            "Request showed that bank owns height {}."
            lastBlockHeight
    when (walletHeight > lastBlockHeight) $
        throwM $
        StorageError $
        W.InternalError $
        format'
            ("Last block height in wallet ({}) is greater than last " <>
             "block's height in bank ({}). Critical error.")
            (walletHeight, lastBlockHeight)
    when (lastBlockHeight /= walletHeight) $
        forM_
            [walletHeight + 1 .. lastBlockHeight]
            (\h ->
                  do verboseSay $ formatSingle' "Updating to height {} ..." h
                     updateToBlockHeight st h
                     verboseSay $ formatSingle' "updated to height {}" h)
    return $ lastBlockHeight /= walletHeight
  where
    verboseSay t = when verbose $ liftIO $ TIO.putStrLn t

-- | Gets amount of coins on user address
getAmount :: WorkMode m => A.RSCoinUserState -> W.UserAddress -> m C.Coin
getAmount st userAddress = do
    -- try to update, but silently fail if net is down
    (_ :: Either SomeException Bool) <- try $ updateBlockchain st False
    getAmountNoUpdate st userAddress

-- | Gets current amount on all accounts user posesses
getUserTotalAmount :: WorkMode m => A.RSCoinUserState -> m C.Coin
getUserTotalAmount st = do
    addrs <- query' st A.GetAllAddresses
    void $ updateBlockchain st False
    sum <$> mapM (getAmountNoUpdate st) addrs

-- | Get amount without storage update
getAmountNoUpdate
    :: WorkMode m
    => A.RSCoinUserState -> W.UserAddress -> m C.Coin
getAmountNoUpdate st userAddress =
    sum . map getValue <$> query' st (A.GetTransactions userAddress)
  where
    getValue = C.getAmountByAddress $ W.toAddress userAddress

-- | Gets amount of coins on user address, chosen by id (∈ 1..n, where
-- n is the number of accounts stored in wallet)
getAmountByIndex :: WorkMode m => A.RSCoinUserState -> Int -> m C.Coin
getAmountByIndex st idx = do
    void $ updateBlockchain st False
    addr <- flip atMay idx <$> query' st A.GetAllAddresses
    maybe
        (throwM $
         InputProcessingError "invalid index was given to getAmountByIndex")
        (getAmount st)
        addr

-- | Returns list of public addresses available
getAllPublicAddresses :: WorkMode m => A.RSCoinUserState -> m [C.Address]
getAllPublicAddresses st = map C.Address <$> query' st A.GetPublicAddresses

-- | Returns transaction history that wallet holds
getTransactionsHistory :: WorkMode m => A.RSCoinUserState -> m [W.TxHistoryRecord]
getTransactionsHistory st = query' st A.GetTxsHistory

-- | Forms transaction given just amount of money to use. Tries to
-- spend coins from accounts that have the least amount of money.
formTransactionFromAll
    :: WorkMode m
    => A.RSCoinUserState -> C.Address -> C.Coin -> m ()
formTransactionFromAll st addressTo amount = do
    addrs <- query' st A.GetAllAddresses
    (amountsWithCoins :: [(Word, C.Coin)]) <-
        filter ((>= 0) . snd) <$>
        mapM (\i -> (fromInteger $ toInteger i+1,)
                    <$> getAmountByIndex st i) [0..length addrs-1]
    totalAmount <- getUserTotalAmount st
    when (amount > totalAmount) $
        throwM $
        InputProcessingError $
        format'
            "Tried to form transaction with amount ({}) greater than available ({})."
            (amount, totalAmount)
    let discoverAmount _ r@(left,_) | left <= 0 = r
        discoverAmount e@(i,c) (left, results) =
            let newLeft = left - c
            in if newLeft < 0
               then (newLeft, (i,left) : results)
               else (newLeft, e : results)
        (_, chosen) =
            foldr discoverAmount (amount, []) $ sortOn snd amountsWithCoins
    liftIO $ putStrLn ("Transaction chosen: " ++ show chosen)
    formTransactionRetry 3 st chosen addressTo amount

-- | Forms transaction out of user input and sends it to the net.
formTransaction
    :: WorkMode m
    => A.RSCoinUserState -> [(Word, C.Coin)] -> C.Address -> C.Coin -> m ()
formTransaction = formTransactionRetry 1

-- | Forms transaction out of user input and sends it. If failure
-- occurs, waits for 600 mcs, then retries up to given amout of tries.
formTransactionRetry
    :: WorkMode m
    => Int
    -> A.RSCoinUserState
    -> [(Word, C.Coin)]
    -> C.Address
    -> C.Coin
    -> m ()
formTransactionRetry _ _ [] _ _ =
    commitError "You should enter at least one source input"
formTransactionRetry tries _ _ _ _ | tries < 1 =
    error "User.Operations.formTransactionRetry shouldn't be called with tries < 1"
formTransactionRetry tries st inputs outputAddr outputCoin =
    run `catch` catcher
  where
    catcher e@FailedToCommit | tries == 1 = throwM e
    catcher FailedToCommit = do
        C.logWarning C.userLoggerName $
            formatSingle'
            "formTransactionRetry failed (FailedToCommit), retries left: {}"
            tries
        wait $ for 600 mcs
        formTransactionRetry (tries-1) st inputs outputAddr outputCoin
    catcher e = throwM e
    run = do
        C.logInfo C.userLoggerName $
            format'
                "Form a transaction from {}, to {}, amount {}"
                ( listBuilderJSONIndent 2 $ map pairBuilder inputs
                , outputAddr
                , outputCoin)
        when (nubBy ((==) `on` fst) inputs /= inputs) $
            commitError "All input addresses should have distinct IDs."
        unless (all (> 0) $ map snd inputs) $
            commitError $
            formatSingle'
                "All input values should be positive, but encountered {}, that's not." $
            head $ filter (<= 0) $ map snd inputs
        accounts <- query' st GetAllAddresses
        let notInRange i = i <= 0 || i > genericLength accounts
        when (any notInRange $ map fst inputs) $
            commitError $
            format'
                "Found an account id ({}) that's not in [1..{}]"
                (head $ filter notInRange $ map fst inputs, length accounts)
        let accInputs :: [(Word, W.UserAddress, C.Coin)]
            accInputs =
                map
                    (\(i,c) ->
                          (i, accounts `genericIndex` (i - 1), c))
                    inputs
            hasEnoughFunds (i,acc,c) = do
                amount <- getAmountNoUpdate st acc
                return $
                    if amount >= c
                        then Nothing
                        else Just i
        overSpentAccounts <-
            filterM
                (\a ->
                      isJust <$> hasEnoughFunds a)
                accInputs
        unless (null overSpentAccounts) $
            commitError $
            (if length overSpentAccounts > 1
                 then "At least the"
                 else "The") <>
            formatSingle'
                " following account doesn't have enough coins: {}"
                (sel1 $ head overSpentAccounts)
        (addrPairList,outTr) <-
            liftIO $
            foldl1 mergeTransactions <$> mapM formTransactionMapper accInputs
        liftIO $
            TIO.putStrLn $ formatSingle' "Please check your transaction: {}" outTr
        void $ updateBlockchain st True
        walletHeight <- query' st A.GetLastBlockId
        lastBlockHeight <- pred <$> C.getBlockchainHeight
        when (walletHeight /= lastBlockHeight) $
            commitError $
            format'
                ("Wallet isn't updated (lastBlockHeight {} when blockchain's last block is {}). " <>
                 "Please synchonize it with blockchain. The transaction wouldn't be sent.")
                (walletHeight, lastBlockHeight)
        let signatures =
                M.fromList $
                map
                    (\(addrid',address') ->
                          (addrid', C.sign (address' ^. W.privateAddress) outTr))
                    addrPairList
        validateTransaction outTr signatures $ lastBlockHeight + 1
        update' st $ A.AddTemporaryTransaction (lastBlockHeight + 1) outTr
    formTransactionMapper
        :: (Word, W.UserAddress, C.Coin)
        -> IO ([(C.AddrId, W.UserAddress)], C.Transaction)
    formTransactionMapper (_,a,c) = do
        (addrids :: [C.AddrId]) <-
            concatMap (C.getAddrIdByAddress $ W.toAddress a) <$>
            query' st (A.GetTransactions a)
        let (chosen,leftCoin) = C.chooseAddresses addrids c
            transaction =
                C.Transaction chosen $
                (outputAddr, outputCoin) :
                (if leftCoin == 0
                     then []
                     else [(W.toAddress a, leftCoin)])
            addrPairList = map (, a) chosen
        return (addrPairList, transaction)
    mergeTransactions
        :: ([(C.AddrId, W.UserAddress)], C.Transaction)
        -> ([(C.AddrId, W.UserAddress)], C.Transaction)
        -> ([(C.AddrId, W.UserAddress)], C.Transaction)
    mergeTransactions (s1,a) (s2,b) =
        ( nub $ s1 <> s2
        , C.Transaction
              (C.txInputs a <> C.txInputs b)
              (nub $ C.txOutputs a <> C.txOutputs b))
