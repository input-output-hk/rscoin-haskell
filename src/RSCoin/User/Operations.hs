{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Basic operations with wallet.

module RSCoin.User.Operations
       ( commitError
       , getAmount
       , getAmountByIndex
       , updateToBlockHeight
       , updateBlockchain
       , formTransaction
       ) where

import           Control.Lens           ((^.))
import           Control.Monad          (filterM, forM_, unless, when)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Acid.Advanced     (query', update')
import           Data.Function          (on)
import           Data.Int               (Int64)
import           Data.List              (nub, nubBy)
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
import           RSCoin.Timed           (WorkMode)
import           RSCoin.User.AcidState  (GetAllAddresses (..))
import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Error      (UserError (..))
import           RSCoin.User.Logic      (validateTransaction)
import qualified RSCoin.User.Wallet     as W

commitError :: MonadThrow m => T.Text -> m ()
commitError = throwM . InputProcessingError

getAmount :: MonadIO m => A.RSCoinUserState -> W.UserAddress -> m C.Coin
getAmount st userAddress =
    sum . map getValue <$> query' st (A.GetTransactions userAddress)
  where
    getValue =
        C.getAmountByAddress $ W.toAddress userAddress

getAmountByIndex :: MonadIO m => A.RSCoinUserState -> Int -> m C.Coin
getAmountByIndex st idx = do
    addr <- flip atMay idx <$> query' st A.GetAllAddresses
    maybe
        (liftIO $
         throwM $
         InputProcessingError "invalid index was given to getAmountByIndex")
        (getAmount st)
        addr

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
    walletHeight <- liftIO $ query' st A.GetLastBlockId
    verboseSay $ formatSingle'
                 "Current known blockchain's height (last HBLock's id) is {}."
                 walletHeight
    lastBlockHeight <- pred <$> C.getBlockchainHeight
    when (walletHeight > lastBlockHeight) $
        throwM $
        StorageError $
        W.InternalError $
        format'
            ("Last block height in wallet ({}) is greater than last " <>
             "block's height in bank ({}). Critical error.")
            (walletHeight, lastBlockHeight)
    if lastBlockHeight == walletHeight
        then return False
        else do
            forM_
                [walletHeight + 1 .. lastBlockHeight]
                (\h ->
                      do verboseSay $ formatSingle' "Updating to height {} ..." h
                         updateToBlockHeight st h
                         verboseSay $ formatSingle' "updated to height {}" h)
            return True
  where
    verboseSay t = when verbose $ liftIO $ TIO.putStr t

-- | Forms transaction out of user input and sends it to the net.
formTransaction :: WorkMode m =>
    A.RSCoinUserState -> [(Int, Int64)] -> C.Address -> C.Coin -> m ()
formTransaction _ [] _ _ =
    commitError "You should enter at least one source input"
formTransaction st inputs outputAddr outputCoin = do
    C.logInfo $
        format' "Form a transaction from {}, to {}, ammount {}" (listBuilderJSONIndent 2 $ map pairBuilder inputs, outputAddr, outputCoin)
    when
        (nubBy ((==) `on` fst) inputs /= inputs) $
        commitError "All input addresses should have distinct IDs."
    unless (all (> 0) $ map snd inputs) $
        commitError $
        formatSingle'
            "All input values should be positive, but encountered {}, that's not." $
        head $ filter (<= 0) $ map snd inputs
    accounts <- query' st GetAllAddresses
    let notInRange i = i <= 0 || i > length accounts
    when
        (any notInRange $ map fst inputs) $
        commitError $
        format'
            "Found an account id ({}) that's not in [1..{}]"
            ( head $ filter notInRange $ map fst inputs
            , length accounts)
    let accInputs :: [(Int, W.UserAddress, C.Coin)]
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
            map (\(addrid',address') ->
                      (addrid', C.sign (address' ^. W.privateAddress) outTr))
                addrPairList
    validateTransaction outTr signatures $ lastBlockHeight + 1
    update' st $ A.AddTemporaryTransaction outTr
  where
    formTransactionMapper :: (Int, W.UserAddress, C.Coin)
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
