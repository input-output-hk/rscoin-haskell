{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Module that provides some functions that transform
-- UserOptions.UserCommand s to IO actions.

module Actions (proceedCommand) where

import           Control.Lens          ((^.))
import           Control.Monad         (filterM, forM_, unless, void, when)
import           Control.Monad.Catch   (MonadThrow, throwM)
import           Control.Monad.Trans   (liftIO)
import           Data.Acid             (query, update)
import           Data.Int              (Int64)
import           Data.Function         (on)
import           Data.List             (nub, nubBy)
import qualified Data.Map              as M
import           Data.Maybe            (fromJust, isJust)
import           Data.Monoid           ((<>))
import           Data.Ord              (comparing)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Tuple.Select     (sel1)

import           Serokell.Util.Text    (format', formatSingle')

import           RSCoin.Core           as C
import           RSCoin.Test           (WorkMode)
import           RSCoin.User.AcidState (GetAllAddresses (..))
import qualified RSCoin.User.AcidState as A
import           RSCoin.User.Error     (UserError (..), eWrap)
import           RSCoin.User.Logic     (validateTransaction)
import qualified RSCoin.User.Wallet    as W
import qualified UserOptions           as O

commitError :: MonadThrow m => T.Text -> m ()
commitError = throwM . InputProcessingError

getAmount :: A.RSCoinUserState -> W.UserAddress -> IO C.Coin
getAmount st userAddress =
    sum . map getValue <$> query st (A.GetTransactions userAddress)
  where
    getValue =
        C.getAmountByAddress $ W.toAddress userAddress

-- | Given the state of program and command, makes correspondent
-- actions.
proceedCommand :: WorkMode m => A.RSCoinUserState -> O.UserCommand -> m ()
proceedCommand st O.ListAddresses =
    liftIO $ eWrap $ 
    do (wallets :: [(C.PublicKey, C.Coin)]) <-
           mapM (\w -> (w ^. W.publicAddress, ) <$> getAmount st w) =<<
           query st GetAllAddresses
       TIO.putStrLn "Here's the list of your accounts:"
       TIO.putStrLn "# | Public ID                                    | Amount"
       mapM_ (TIO.putStrLn . format' "{}.  {} : {}") $
           uncurry (zip3 [(1 :: Integer) ..]) $ unzip wallets
proceedCommand st (O.FormTransaction inputs outputAddrStr) =
    eWrap $
    do let pubKey = C.Address <$> C.constructPublicKey outputAddrStr
       unless (isJust pubKey) $
           commitError $
           "Provided key can't be exported: " <> outputAddrStr
       formTransaction st inputs (fromJust pubKey) $
           C.Coin (sum $ map snd inputs)
proceedCommand st O.UpdateBlockchain =
    eWrap $ 
    do walletHeight <- liftIO $ query st A.GetLastBlockId
       liftIO $ TIO.putStrLn $
           formatSingle'
               "Current known blockchain's height (last HBLock's id) is {}."
               walletHeight
       lastBlockHeight <- pred <$> getBlockchainHeight
       when (walletHeight > lastBlockHeight) $
           throwM $
           StorageError $
           W.InternalError $
           format'
               ("Last block height in wallet ({}) is greater than last " <>
                "block's height in bank ({}). Critical error.")
               (walletHeight, lastBlockHeight)
       if lastBlockHeight == walletHeight
           then liftIO $ putStrLn "Blockchain is updated already."
           else do  
               forM_
                  [walletHeight + 1 .. lastBlockHeight]
                  (updateToBlockHeight st)
               liftIO $ TIO.putStrLn "Successfully updated blockchain!"
proceedCommand _ (O.Dump command) = eWrap $ dumpCommand command

dumpCommand :: WorkMode m => O.DumpCommand -> m ()
dumpCommand (O.DumpHBlocks from to) =
    void $ C.getBlocks from to
dumpCommand (O.DumpHBlock pId) =
    void $ C.getBlockByHeight pId
dumpCommand O.DumpMintettes =
    void $ C.getMintettes
dumpCommand O.DumpPeriod =
    void $ C.getBlockchainHeight
dumpCommand (O.DumpLogs mId from to) =
    void $ C.getLogs mId from to
dumpCommand (O.DumpMintetteUtxo mId) =
    void $ C.getMintetteUtxo mId
dumpCommand (O.DumpMintetteBlocks mId pId) =
    void $ C.getMintetteBlocks mId pId
dumpCommand (O.DumpMintetteLogs mId pId) =
    void $ C.getMintetteLogs mId pId

-- | Updates wallet to given blockchain height assuming that it's in
-- previous height state already.
updateToBlockHeight :: WorkMode m => A.RSCoinUserState -> PeriodId -> m ()
updateToBlockHeight st newHeight = do
    liftIO $ TIO.putStr $ formatSingle' "Updating to height {} ... " newHeight
    C.HBlock{..} <- getBlockByHeight newHeight
    -- TODO validate this block with integrity check that we don't have
    liftIO $ update st $ A.WithBlockchainUpdate newHeight hbTransactions
    liftIO $ TIO.putStrLn $ formatSingle' " updated to height {}" newHeight

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
