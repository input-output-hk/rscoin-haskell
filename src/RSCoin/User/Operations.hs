{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Basic operations with wallet.

module RSCoin.User.Operations
       ( walletInitialized
       , commitError
       , getUserTotalAmount
       , getAmount
       , getAmountNoUpdate
       , getAmountByIndex
       , getAllPublicAddresses
       , getTransactionsHistory
       , updateToBlockHeight
       , updateBlockchain
       , formTransactionFromAll
       , formTransaction
       , formTransactionRetry
       ) where

import           Control.Exception      (SomeException, assert, fromException)
import           Control.Lens           ((^.))
import           Control.Monad          (filterM, forM_, unless, void, when)
import           Control.Monad.Catch    (MonadThrow, catch, throwM, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Acid.Advanced     (query', update')
import           Data.Function          (on)
import           Data.List              (genericIndex, genericLength, nubBy,
                                         sortOn)
import qualified Data.Map               as M
import           Data.Maybe             (isJust)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.IO           as TIO
import           Data.Tuple.Select      (sel1)
import           Safe                   (atMay)

import           Serokell.Util          (format', formatSingle',
                                         listBuilderJSON, listBuilderJSONIndent,
                                         pairBuilder)

import qualified RSCoin.Core            as C
import           RSCoin.Mintette        (MintetteError (MEInactive))
import           RSCoin.Timed           (WorkMode, for, sec, wait)
import           RSCoin.User.AcidState  (GetAllAddresses (..))
import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Cache      (UserCache)
import           RSCoin.User.Error      (UserError (..), UserLogicError,
                                         isWalletSyncError)
import           RSCoin.User.Logic      (validateTransaction)
import qualified RSCoin.User.Wallet     as W

walletInitialized :: MonadIO m => A.RSCoinUserState -> m Bool
walletInitialized st = query' st A.IsInitialized

commitError :: (MonadIO m, MonadThrow m) => T.Text -> m a
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
getAmount :: WorkMode m => A.RSCoinUserState -> W.UserAddress -> m (M.Map C.Color C.Coin)
getAmount st userAddress = do
    -- try to update, but silently fail if net is down
    (_ :: Either SomeException Bool) <- try $ updateBlockchain st False
    getAmountNoUpdate st userAddress

-- | Gets current amount on all accounts user posesses. Boolean flag
-- stands for "if update blockchain inside"
getUserTotalAmount :: WorkMode m => Bool -> A.RSCoinUserState -> m (M.Map C.Color C.Coin)
getUserTotalAmount upd st = do
    addrs <- query' st A.GetAllAddresses
    when upd $ void $ updateBlockchain st False
    C.mergeCoinsMaps <$> mapM (getAmountNoUpdate st) addrs

-- | Get amount without storage update
getAmountNoUpdate
    :: WorkMode m
    => A.RSCoinUserState -> W.UserAddress -> m (M.Map C.Color C.Coin)
getAmountNoUpdate st userAddress =
    C.mergeCoinsMaps . map getCoins <$> query' st (A.GetTransactions userAddress)
  where
    getCoins = C.getAmountByAddress $ W.toAddress userAddress

-- | Gets amount of coins on user address, chosen by id (∈ 1..n, where
-- n is the number of accounts stored in wallet)
getAmountByIndex :: WorkMode m => A.RSCoinUserState -> Int -> m (M.Map C.Color C.Coin)
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
-- Supports uncolored coins only (by design)
formTransactionFromAll
    :: WorkMode m
    => A.RSCoinUserState
    -> Maybe UserCache
    -> C.Address
    -> C.Coin
    -> m C.Transaction
formTransactionFromAll st maybeCache addressTo amount =
    assert (C.getColor amount == 0) $ do
    addrs <- query' st A.GetAllAddresses
    (indecesWithCoins :: [(Word, C.Coin)]) <-
        filter ((>= 0) . snd) <$>
        mapM (\i -> (fromInteger $ toInteger i+1,) .
                    M.findWithDefault 0 0 <$>
                    getAmountByIndex st i)
             [0..length addrs-1]
    let totalAmount = C.sumCoin $ map snd indecesWithCoins
    when (amount > totalAmount) $
        throwM $
        InputProcessingError $
        format'
            "Tried to form transaction with amount ({}) greater than available ({})."
            (amount, totalAmount)
    let discoverAmount _ r@(left,_)
          | left <= 0 = r
        discoverAmount e@(i,c) (left,results) =
            let newLeft = left - c
            in if newLeft < 0
               then (newLeft, (i,left) : results)
               else (newLeft, e : results)
        (_, chosen) =
            foldr discoverAmount (amount, []) $ sortOn snd indecesWithCoins
    liftIO $ putStrLn ("Transaction chosen: " ++ show chosen)
    formTransactionRetry 3 st maybeCache True (map (\(a,b) -> (a,[b])) chosen) addressTo [amount]

-- | Forms transaction out of user input and sends it to the net.
formTransaction
    :: WorkMode m
    => A.RSCoinUserState
    -> Maybe UserCache
    -> Bool
    -> [(Word, [C.Coin])]
    -> C.Address
    -> [C.Coin]
    -> m C.Transaction
formTransaction = formTransactionRetry 1

-- | Forms transaction out of user input and sends it. If failure
-- occurs, waits for 600 mcs, then retries up to given amout of tries.
formTransactionRetry
    :: WorkMode m
    => Int                 -- ^ Number of retries
    -> A.RSCoinUserState   -- ^ RSCoin user state (acid)
    -> Maybe UserCache
    -> Bool                -- ^ Verbosity flag
    -> [(Word, [C.Coin])]  -- ^ Inputs as (wallet # ∈ [1..], list of coins (nonempty!)
    -> C.Address           -- ^ Address to send money to
    -> [C.Coin]            -- ^ List of coins to send, mb empty, then will be calculated from inputs
    -> m C.Transaction
formTransactionRetry _ _ _ _ [] _ _ =
    commitError "You should enter at least one source input"
formTransactionRetry tries _ _ _ _ _ _ | tries < 1 =
    error "User.Operations.formTransactionRetry shouldn't be called with tries < 1"
formTransactionRetry tries st maybeCache verbose inputs outputAddr outputCoin =
    run `catch` catcher
  where
    catcher
        :: WorkMode m
        => SomeException -> m C.Transaction
    catcher e
      | isRetriableException e && tries > 1 = logMsgAndRetry e
      | otherwise = throwM e
    isRetriableException :: SomeException -> Bool
    isRetriableException e
      | Just (_ :: UserLogicError) <- fromException e = True
      | Just MEInactive <- fromException e = True
      | isWalletSyncError e = True
      | otherwise = False
    logMsgAndRetry
        :: (WorkMode m, Buildable s)
        => s -> m C.Transaction
    logMsgAndRetry msg = do
        C.logWarning C.userLoggerName $
            format'
                "formTransactionRetry failed ({}), retries left: {}"
                (msg, tries - 1)
        wait $ for 1 sec
        formTransactionRetry
            (tries - 1)
            st
            maybeCache
            verbose
            inputs
            outputAddr
            outputCoin
    run
        :: WorkMode m
        => m C.Transaction
    run = do
        () <$ updateBlockchain st verbose
        C.logInfo C.userLoggerName $
            format'
                "Form a transaction from {}, to {}, amount {}"
                ( listBuilderJSONIndent 2 $
                      map (\(a, b) -> pairBuilder (a, listBuilderJSON b)) inputs
                , outputAddr
                , listBuilderJSONIndent 2 $ outputCoin)
        when (nubBy ((==) `on` fst) inputs /= inputs) $
            commitError "All input addresses should have distinct IDs."
        unless (all (> 0) $ concatMap snd inputs) $
            commitError $
            formatSingle'
                "All input values should be positive, but encountered {}, that's not." $
            head $ filter (<= 0) $ concatMap snd inputs
        accounts <- query' st GetAllAddresses
        let notInRange i = i <= 0 || i > genericLength accounts
        when (any notInRange $ map fst inputs) $
            commitError $
            format'
                "Found an account id ({}) that's not in [1..{}]"
                (head $ filter notInRange $ map fst inputs, length accounts)
        let accInputs :: [(Word, W.UserAddress, M.Map C.Color C.Coin)]
            accInputs =
                map (\(i,c) -> (i, accounts `genericIndex` (i - 1), C.coinsToMap c))
                    inputs
--            hasEnoughFunds :: (Word, W.UserAddress, M.Map C.Color C.Coin) -> m Maybe Word
            hasEnoughFunds (i,acc,coinsMap) = do
                amountMap <- getAmountNoUpdate st acc
                let sufficient =
                        all (\col0 ->
                              let weHave = M.findWithDefault 0 col0 amountMap
                                  weSpend = M.findWithDefault 0 col0 coinsMap
                              in weHave >= weSpend)
                            (M.keys coinsMap)
                return $ if sufficient then Nothing else Just i
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
        (addrPairList,inputAddrids,outputs) <-
            liftIO $
            foldl1 pair3merge <$>
            mapM formTransactionMapper (map (\(_,b,c) -> (b,c)) accInputs)
        let outTr = C.Transaction { txInputs = inputAddrids
                                  , txOutputs = outputs ++ (map (outputAddr,) outputCoin)
                                  }
        when (not (null outputCoin) && not (C.validateSum outTr)) $
            commitError $
            formatSingle' "Your transaction doesn't pass validity check: {}" outTr
        when (null outputCoin && not (C.validateSum outTr)) $
            commitError $
            formatSingle'
                "Our code is broken and our auto-generated transaction is invalid: {}"
                outTr
        verboseSay $ formatSingle' "Please check your transaction: {}" outTr
        walletHeight <- query' st A.GetLastBlockId
        lastBlockHeight <- pred <$> C.getBlockchainHeight
        when (walletHeight /= lastBlockHeight) $
            throwM $
            WalletSyncError $
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
        validateTransaction maybeCache outTr signatures $ lastBlockHeight + 1
        update' st $ A.AddTemporaryTransaction (lastBlockHeight + 1) outTr
        return outTr
    -- For given address and coins to send from it it returns a
    -- pair. First element is chosen addrids with user addresses to
    -- make signature map afterwards. The second is inputs of
    -- transaction, third is change outputs
    formTransactionMapper
        :: (W.UserAddress, M.Map C.Color C.Coin)
        -> IO ([(C.AddrId, W.UserAddress)], [C.AddrId], [(C.Address, C.Coin)])
    formTransactionMapper (address, requestedCoins) = do
        (addrids :: [C.AddrId]) <-
            concatMap (C.getAddrIdByAddress $ W.toAddress address) <$>
            query' st (A.GetTransactions address)
        let -- Pairs of chosen addrids and change for each color
            chosenMap :: [([C.AddrId], C.Coin)]
            chosenMap = M.elems $ C.chooseAddresses addrids requestedCoins
            -- All addrids from chosenMap
            inputAddrids = concatMap fst chosenMap
            -- Non-null changes (coins) from chosenMap
            changesValues :: [C.Coin]
            changesValues = filter (/= 0) $ map snd chosenMap
            changes :: [(C.Address, C.Coin)]
            changes = map (W.toAddress address,) changesValues
            autoGeneratedOutputs :: [(C.Address, C.Coin)]
            autoGeneratedOutputs =
                if null outputCoin
                then map (outputAddr,) $ C.coinsToList requestedCoins
                else []
        return ( map (,address) inputAddrids
               , inputAddrids
               , changes ++ autoGeneratedOutputs
               )
    pair3merge :: ([a], [b], [c]) -> ([a], [b], [c]) -> ([a], [b], [c])
    pair3merge (a,b,c) (d,e,f) = (a++d, b++e, c++f)
    verboseSay t = when verbose $ liftIO $ TIO.putStrLn t
