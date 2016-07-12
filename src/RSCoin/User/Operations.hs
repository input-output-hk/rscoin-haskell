{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Basic operations with wallet.

module RSCoin.User.Operations
       ( A.openState
       , A.openMemState
       , A.closeState
       , A.initState
       , A.initStateBank
       , walletInitialized
       , commitError
       , getUserTotalAmount
       , getAmount
       , getAmountNoUpdate
       , getAmountByIndex
       , getAllPublicAddresses
       , getTransactionsHistory
       , getLastBlockId
       , genesisAddressIndex
       , isInitialized
       , updateToBlockHeight
       , updateBlockchain
       , addAddress
       , submitTransactionFromAll
       , TransactionInput
       , TransactionData (..)
       , submitTransaction
       , submitTransactionRetry
       , createCertificateChain
       ) where

import           Control.Exception      (SomeException, assert, fromException)
import           Control.Monad          (filterM, forM_, unless, void, when)
import           Control.Monad.Catch    (MonadThrow, catch, throwM, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Acid.Advanced     (query', update')
import           Data.Function          (on)
import           Data.List              (elemIndex, genericIndex, genericLength,
                                         nub, nubBy, sortOn)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust, fromMaybe, isJust, isNothing)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.IO           as TIO
import           Data.Tuple.Select      (sel1, sel2, sel3)
import           Formatting             (build, int, sformat, shown, (%))
import           Safe                   (atMay)

import           Serokell.Util          (format', formatSingle',
                                         listBuilderJSON, listBuilderJSONIndent,
                                         pairBuilder)

import qualified RSCoin.Core            as C
import           RSCoin.Timed           (WorkMode, for, sec, wait)
import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Cache      (UserCache)
import           RSCoin.User.Error      (UserError (..), UserLogicError,
                                         isWalletSyncError)
import           RSCoin.User.Logic      (SignatureBundle, getExtraSignatures,
                                         joinBundles, validateTransaction)
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
    hblock <- C.getBlockByHeight newHeight
    -- TODO validate this block with integrity check that we don't have
    update' st $ A.WithBlockchainUpdate newHeight hblock

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
getAmount :: WorkMode m => A.RSCoinUserState -> C.Address -> m C.CoinsMap
getAmount st address = do
    -- try to update, but silently fail if net is down
    (_ :: Either SomeException Bool) <- try $ updateBlockchain st False
    getAmountNoUpdate st address

-- | Gets current amount on all accounts user posesses. Boolean flag
-- stands for "if update blockchain inside"
getUserTotalAmount :: WorkMode m => Bool -> A.RSCoinUserState -> m C.CoinsMap
getUserTotalAmount upd st = do
    addrs <- query' st A.GetOwnedDefaultAddresses
    when upd $ void $ updateBlockchain st False
    C.mergeCoinsMaps <$> mapM (getAmountNoUpdate st) addrs

-- | Get amount without storage update
getAmountNoUpdate
    :: WorkMode m
    => A.RSCoinUserState -> C.Address -> m C.CoinsMap
getAmountNoUpdate st address =
    C.coinsToMap . map sel3 <$> query' st (A.GetOwnedAddrIds address)

-- | Gets amount of coins on user address, chosen by id (∈ 1..n, where
-- n is the number of accounts stored in wallet)
getAmountByIndex :: WorkMode m => A.RSCoinUserState -> Int -> m C.CoinsMap
getAmountByIndex st idx = do
    void $ updateBlockchain st False
    addr <- flip atMay idx <$> query' st A.GetOwnedDefaultAddresses
    maybe
        (throwM $
         InputProcessingError "invalid index was given to getAmountByIndex")
        (getAmount st)
        addr

-- | Returns list of public addresses available
getAllPublicAddresses :: WorkMode m => A.RSCoinUserState -> m [C.Address]
getAllPublicAddresses st = query' st A.GetOwnedDefaultAddresses

genesisAddressIndex :: WorkMode m => A.RSCoinUserState -> m (Maybe Word)
genesisAddressIndex st = do
    adrList <- getAllPublicAddresses st
    return $
        fromIntegral <$>
        elemIndex C.genesisAddress adrList

-- | Returns transaction history that wallet holds
getTransactionsHistory
    :: MonadIO m
    => A.RSCoinUserState -> m [W.TxHistoryRecord]
getTransactionsHistory st = query' st A.GetTxsHistory

getLastBlockId :: MonadIO m => A.RSCoinUserState -> m Int
getLastBlockId st = query' st A.GetLastBlockId

isInitialized :: MonadIO m => A.RSCoinUserState -> m Bool
isInitialized st = query' st A.IsInitialized

-- | Puts given address and it's related transactions (that contain it
-- as output S_{out}) into wallet. Blockchain won't be queried.
addAddress
    :: MonadIO m
    => A.RSCoinUserState -> C.SecretKey -> C.PublicKey -> [C.Transaction] -> C.PeriodId -> m ()
addAddress st sk pk ts = update' st . A.AddAddress (C.Address pk, sk) ts

-- | Forms transaction given just amount of money to use. Tries to
-- spend coins from accounts that have the least amount of money.
-- Supports uncolored coins only (by design)
submitTransactionFromAll
    :: WorkMode m
    => A.RSCoinUserState
    -> Maybe UserCache
    -> C.Address
    -> C.Coin
    -> m C.Transaction
submitTransactionFromAll st maybeCache addressTo amount =
    assert (C.getColor amount == 0) $
    do addrs <- query' st A.GetOwnedDefaultAddresses
       (indicesWithCoins :: [(Word, C.Coin)]) <-
           filter (not . C.isNegativeCoin . snd) <$>
           mapM
               (\i ->
                     (fromIntegral i + 1, ) . M.findWithDefault 0 0 <$>
                     getAmountByIndex st i)
               [0 .. length addrs - 1]
       let totalAmount = C.sumCoin $ map snd indicesWithCoins
       when (amount > totalAmount) $
           throwM $
           InputProcessingError $
           format'
               "Tried to form transaction with amount ({}) greater than available ({})."
               (amount, totalAmount)
       let discoverAmount _ r@(left,_)
             | not $ C.isPositiveCoin left = r
           discoverAmount e@(i,c) (left,results) =
               let newLeft = left - c
               in if newLeft < 0
                      then (newLeft, (i, left) : results)
                      else (newLeft, e : results)
           (_,chosen) =
               foldr discoverAmount (amount, []) $ sortOn snd indicesWithCoins
           td =
               TransactionData
               { tdInputs = map
                     (\(a,b) ->
                           (a, [b]))
                     chosen
               , tdOutputAddress = addressTo
               , tdOutputCoins = [amount]
               }
       C.logInfo C.userLoggerName $
           sformat ("Transaction chosen: " % shown) chosen
       submitTransactionRetry 3 st maybeCache td

-- | A single input used to form transaction. It consists of two
-- things: index of wallet (zero-based) and non-empty list of coins to send.
type TransactionInput = (Word, [C.Coin])

data TransactionData = TransactionData
    {
      -- | List of inputs, see `FormTransactionInput` description.
      tdInputs        :: [TransactionInput]
      -- | Output address where to send coins.
    , tdOutputAddress :: C.Address
      -- | List of coins to send. It may be empty, then will be
      -- calculated from inputs. Passing non-empty list is useful if
      -- one wants to color coins.
    , tdOutputCoins   :: [C.Coin]
    } deriving (Show)

-- | Forms transaction out of user input and sends it to the net.
submitTransaction
    :: WorkMode m
    => A.RSCoinUserState
    -> Maybe UserCache
    -> TransactionData
    -> m C.Transaction
submitTransaction = submitTransactionRetry 1

-- | Forms transaction out of user input and sends it. If failure
-- occurs, waits for 1 sec, then retries up to given amout of tries.
submitTransactionRetry
    :: forall m.
       WorkMode m
    => Word               -- ^ Number of retries
    -> A.RSCoinUserState  -- ^ RSCoin user state (acid)
    -> Maybe UserCache    -- ^ Optional cache to decrease number of RPC
    -> TransactionData    -- ^ Transaction description.
    -> m C.Transaction
submitTransactionRetry tries st maybeCache td@TransactionData{..}
  | tries < 1 =
      error
          "User.Operations.submitTransactionRetry shouldn't be called with tries < 1"
  | null tdInputs = commitError "you should enter at least one source input"
  | otherwise = do
      (tx,signatures) <- constructAndSignTransaction st td
      tx <$ sendTransactionRetry tries st maybeCache tx signatures

constructAndSignTransaction
    :: forall m.
       WorkMode m
    => A.RSCoinUserState -> TransactionData -> m (C.Transaction, SignatureBundle)
constructAndSignTransaction st TransactionData{..} = do
    () <$ updateBlockchain st False
    C.logInfo C.userLoggerName $
        format'
            "Form a transaction from {}, to {}, amount {}"
            ( listBuilderJSONIndent 2 $
              map
                  (\(a,b) ->
                        pairBuilder (a, listBuilderJSON b))
                  tdInputs
            , tdOutputAddress
            , listBuilderJSONIndent 2 $ tdOutputCoins)
    when (nubBy ((==) `on` fst) tdInputs /= tdInputs) $
        commitError "All input addresses should have distinct indices."
    unless (all C.isPositiveCoin $ concatMap snd tdInputs) $
        commitError $
        formatSingle'
            "All input values should be positive, but encountered {}, that's not." $
        head $ filter (<= 0) $ concatMap snd tdInputs
    accounts <- query' st A.GetOwnedAddresses
    let notInRange i = i >= genericLength accounts
    when (any notInRange $ map fst tdInputs) $
        commitError $
        sformat
            ("Found an address id (" % int % ") that's not in [0 .. " % int %
             ")")
            (head $ filter notInRange $ map fst tdInputs)
            (length accounts)
    let accInputs :: [(C.Address, M.Map C.Color C.Coin)]
        accInputs =
            map
                (\(i,c) ->
                      (accounts `genericIndex` i, C.coinsToMap c))
                tdInputs
        hasEnoughFunds :: (C.Address, C.CoinsMap) -> m Bool
        hasEnoughFunds (acc,coinsMap) = do
            amountMap <- getAmountNoUpdate st acc
            let sufficient =
                    all
                        (\col0 ->
                              let weHave = M.findWithDefault 0 col0 amountMap
                                  weSpend = M.findWithDefault 0 col0 coinsMap
                              in weHave >= weSpend)
                        (M.keys coinsMap)
            return sufficient
    overSpentAccounts <- filterM (fmap not . hasEnoughFunds) accInputs
    unless (null overSpentAccounts) $
        commitError $
        (if length overSpentAccounts > 1
             then "At least the"
             else "The") <>
        formatSingle'
            " following account doesn't have enough coins: {}"
            (sel1 $ head overSpentAccounts)
    txPieces <-
        liftIO $
        mapM
            (uncurry (submitTransactionMapper st tdOutputCoins tdOutputAddress))
            accInputs
    when (any isNothing txPieces) $
        commitError "Couldn't form transaction. Not enough funds."
    let (addrPairList,inputAddrids,outputs) =
            foldl1 pair3merge $ map fromJust txPieces
        outTr =
            C.Transaction
            { txInputs = inputAddrids
            , txOutputs = outputs ++ map (tdOutputAddress, ) tdOutputCoins
            }
    signatures <-
            M.fromList <$>
            mapM
                (\(addrid',address') -> do
                      (pK, sK) <- fromJust <$> query' st (A.FindUserAddress address')
                      return (addrid', (pK, C.sign sK outTr)))
                addrPairList
    when (not (null tdOutputCoins) && not (C.validateSum outTr)) $
        commitError $
        formatSingle' "Your transaction doesn't pass validity check: {}" outTr
    when (null tdOutputCoins && not (C.validateSum outTr)) $
        commitError $
        formatSingle'
            "Our code is broken and our auto-generated transaction is invalid: {}"
            outTr
    sigBundle <- M.fromList <$> mapM toSignatureBundle (M.assocs signatures)
    return (outTr, sigBundle)
  where
    toSignatureBundle (addrid,signPair) = do
        address <- fromMaybe (error "Exception1 at toSignatureBundle in Operations.hs")
                   <$> query' st (A.ResolveAddressLocally addrid)
        str <- fromMaybe (error "Exception2 at toSignatureBundle in Operations.hs")
                   <$> query' st (A.GetAddressStrategy address)
        return (addrid, (address, str, [signPair]))
    pair3merge :: ([a], [b], [c]) -> ([a], [b], [c]) -> ([a], [b], [c])
    pair3merge = mappend

-- For given address and coins to send from it it returns a
-- triple. First element is chosen addrids with user addresses to
-- make signature map afterwards. The second is inputs of
-- transaction, third is change outputs
submitTransactionMapper
    :: A.RSCoinUserState
    -> [C.Coin]
    -> C.Address
    -> C.Address
    -> C.CoinsMap
    -> IO (Maybe ([(C.AddrId, C.Address)], [C.AddrId], [(C.Address, C.Coin)]))
submitTransactionMapper st outputCoin outputAddr address requestedCoins = do
    (addrids :: [C.AddrId]) <- query' st (A.GetOwnedAddrIds address)
    let
        -- Pairs of chosen addrids and change for each color
        chosenMap0
            :: Maybe [([C.AddrId], C.Coin)]
        chosenMap0 = M.elems <$> C.chooseAddresses addrids requestedCoins
        chosenMap = fromJust chosenMap0
        -- All addrids from chosenMap
        inputAddrids = concatMap fst chosenMap
        -- Non-null changes (coins) from chosenMap
        changesValues
            :: [C.Coin]
        changesValues = filter (> 0) $ map snd chosenMap
        changes :: [(C.Address, C.Coin)]
        changes = map (address, ) changesValues
        autoGeneratedOutputs :: [(C.Address, C.Coin)]
        autoGeneratedOutputs
          | null outputCoin = map (outputAddr, ) $ C.coinsToList requestedCoins
          | otherwise = []
        retValue =
            ( map (, address) inputAddrids
            , inputAddrids
            , changes ++ autoGeneratedOutputs)
    return $ (const retValue) <$> chosenMap0

sendTransactionRetry
    :: forall m.
       WorkMode m
    => Word
    -> A.RSCoinUserState
    -> Maybe UserCache
    -> C.Transaction
    -> SignatureBundle
    -> m ()
sendTransactionRetry tries st maybeCache tx signatures
  | tries < 1 =
      error
          "User.Operations.sendTransactionRetry shouldn't be called with tries < 1"
  | otherwise = sendTransactionDo st maybeCache tx signatures `catch` handler
  where
    handler :: SomeException -> m ()
    handler e
      | isRetriableException e && tries > 1 = logMsgAndRetry e
      | otherwise = throwM e
    logMsgAndRetry
        :: Buildable s
        => s -> m ()
    logMsgAndRetry msg = do
        C.logWarning C.userLoggerName $
            format'
                "Failed to send transaction ({}), retries left: {}"
                (msg, tries - 1)
        wait $ for 1 sec
        () <$ updateBlockchain st False
        sendTransactionRetry (tries - 1) st maybeCache tx signatures

sendTransactionDo
    :: forall m.
       WorkMode m
    => A.RSCoinUserState
    -> Maybe UserCache
    -> C.Transaction
    -> SignatureBundle
    -> m ()
sendTransactionDo st maybeCache tx signatures = do
    walletHeight <- query' st A.GetLastBlockId
    periodId <- C.getBlockchainHeight
    let lastAppliedBlock = periodId - 1
    C.logInfo C.userLoggerName $
        sformat
            ("Sending transaction: " % build % ", period id is " % int)
            tx
            periodId
    when (walletHeight /= lastAppliedBlock) $
        throwM $
        WalletSyncError $
        format'
            "Wallet isn't updated (lastBlockHeight {} when blockchain's last block is {})."
            (walletHeight, lastAppliedBlock)
    let nonDefaultAddresses =
            M.fromListWith (\(str,a1,sgn) (_,a2,_) -> (str, nub $ a1 ++ a2, sgn)) $
            map (\(addrid,(addr,str,sgns)) -> (addr,(str,[addrid],head $ sgns))) $
            filter ((/= C.DefaultStrategy) . sel2 . snd) $
            M.assocs signatures
    extraSignatures <- getExtraSignatures tx nonDefaultAddresses 120
    let allSignatures :: SignatureBundle
        allSignatures = M.unionWith joinBundles
                                    (fromMaybe M.empty extraSignatures)
                                    signatures
    -- sending transaction only if
    -- it's "distributed signing" case ⇒ extraSignatures decided so
    let willWeSend = null nonDefaultAddresses || isJust extraSignatures
    when willWeSend $ validateTransaction maybeCache tx allSignatures periodId
    update' st $ A.AddTemporaryTransaction periodId tx
    C.logInfo C.userLoggerName $
        if willWeSend
        then "Successfully sent a transaction!"
        else "Somebody has sent a transaction, won't do it. Maybe retry later."

isRetriableException :: SomeException -> Bool
isRetriableException e
    | Just (_ :: UserLogicError) <- fromException e = True
    | isWalletSyncError e = True
    | otherwise = False

-- | This method create certificate chain for user address with
-- help of attain. WARNING! This method is just temporal solution
-- for testing purposes until we will generate IOU.
createCertificateChain :: C.PublicKey -> [(C.Signature, C.PublicKey)]
createCertificateChain userPublicKey =
    [ (C.sign C.attainSecretKey C.attainPublicKey, C.attainPublicKey)  -- @TODO: should introduce `seedPK`
    , (C.sign C.attainSecretKey userPublicKey,     userPublicKey)
    ]
