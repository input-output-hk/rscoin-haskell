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
       , checkAddressId
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
       , addUserAddress
       , importAddress
       , deleteUserAddress
       , submitTransactionFromAll
       , TransactionInput
       , TransactionData (..)
       , submitTransaction
       , submitTransactionRetry
       , retrieveAllocationsList
       ) where

import           Control.Arrow          ((***))
import           Control.Exception      (SomeException, assert, fromException)
import           Control.Lens           ((^.))
import           Control.Monad          (filterM, forM_, unless, void, when)
import           Control.Monad.Catch    (MonadThrow, catch, throwM, try)
import           Control.Monad.Extra    (whenJust)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Acid.Advanced     (query', update')
import           Data.Function          (on)
import qualified Data.IntMap.Strict     as I
import           Data.List              (elemIndex, foldl1', genericIndex,
                                         genericLength, groupBy, nub, sortOn)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust, fromMaybe, isJust, isNothing)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.IO           as TIO
import           Data.Tuple.Select      (sel1, sel2, sel3)
import           Formatting             (build, int, sformat, shown, (%))
import           Safe                   (atMay)

import           Serokell.Util          (listBuilderJSON, listBuilderJSONIndent,
                                         pairBuilder)

import qualified RSCoin.Core            as C
import           RSCoin.Timed           (MonadRpc (getNodeContext), WorkMode,
                                         for, sec, wait)
import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Cache      (UserCache)
import           RSCoin.User.Error      (UserError (..), UserLogicError,
                                         isWalletSyncError)
import           RSCoin.User.Logic      (SignatureBundle, getExtraSignatures,
                                         joinBundles, validateTransaction)
import qualified RSCoin.User.Wallet     as W

walletInitialized :: MonadIO m => A.RSCoinUserState -> m Bool
walletInitialized st = query' st A.IsInitialized

commitError :: (MonadIO m, MonadThrow m, C.WithNamedLogger m) => T.Text -> m a
commitError e = do
    C.logError e
    throwM . InputProcessingError $ e

-- | Checks address id (should be in [0..length allAddrs)) to be so
checkAddressId :: (WorkMode m, Integral n) => A.RSCoinUserState -> n -> m ()
checkAddressId st ix = do
    genAddr <- (^. C.genesisAddress) <$> getNodeContext
    accounts <- query' st $ A.GetOwnedAddresses genAddr
    let notInRange i = i >= genericLength accounts || i < 0
    when (notInRange ix) $
        commitError $
        sformat
            ("Found an address id (" % int % ") that's not in [0 .. " % int %
             ")") ix (length accounts)

-- | Updates wallet to given blockchain height assuming that it's in
-- previous height state already.
updateToBlockHeight :: WorkMode m => A.RSCoinUserState -> C.PeriodId -> m ()
updateToBlockHeight st newHeight = do
    walletHeight <- query' st A.GetLastBlockId
    when (walletHeight >= newHeight) $
        commitError $
        sformat ("updateToBlockchainHeight update to " % int %
                 " was requested while current height is greater (" %
                  int % ")") newHeight walletHeight
    hblocks <- C.getBlocksByHeight (walletHeight+1) newHeight
    -- TODO validate this block with integrity check that we don't have
    forM_ ([walletHeight+1..newHeight] `zip` hblocks) $ \(pid,hblock) ->
        update' st $ A.WithBlockchainUpdate pid hblock

-- | Updates blockchain to the last height (that was requested in this
-- function call). Returns bool indicating that blockchain was or
-- wasn't updated. Does throw exception when update is not possible
-- due to some error. Thus 'False' return means that blockchain wasn't
-- updated and it's ok (already at max height)
updateBlockchain :: WorkMode m => A.RSCoinUserState -> Bool -> m Bool
updateBlockchain st verbose = do
    walletHeight <- query' st A.GetLastBlockId
    verboseSay $
        sformat
            ("Current known blockchain's height (last HBLock's id) is " % build % ".")
            walletHeight
    lastBlockHeight <- pred <$> C.getBlockchainHeight
    verboseSay $
        sformat
            ("Request showed that bank owns height " % build % ".")
            lastBlockHeight
    when (walletHeight > lastBlockHeight) $
        throwM $
        StorageError $
        W.InternalError $
        sformat
            ("Last block height in wallet (" % int % ") is greater than last " %
             "block's height in bank (" % int % "). Critical error.")
            walletHeight lastBlockHeight
    when (lastBlockHeight /= walletHeight) $ do
        let delta = max 100 $ (lastBlockHeight - walletHeight) `div` 10
            periods =
                takeWhile (< lastBlockHeight)
                    (iterate (+delta) (walletHeight + 1)) ++
                [lastBlockHeight]
        forM_ periods
            (\h -> do
                verboseSay $ sformat ("Updating to height " % int % " ...") h
                updateToBlockHeight st h
                verboseSay $ sformat ("Updated to height " % int) h)
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
    genAddr <- (^. C.genesisAddress) <$> getNodeContext
    addrs <- query' st $ A.GetOwnedDefaultAddresses genAddr
    when upd $ void $ updateBlockchain st False
    C.mergeCoinsMaps <$> mapM (getAmountNoUpdate st) addrs

-- | Get amount without storage update
getAmountNoUpdate
    :: WorkMode m
    => A.RSCoinUserState -> C.Address -> m C.CoinsMap
getAmountNoUpdate st address = do
    genAddr <- (^. C.genesisAddress) <$> getNodeContext
    C.coinsToMap . map sel3 <$> query' st (A.GetOwnedAddrIds genAddr address)

-- | Gets amount of coins on user address, chosen by id (∈ 1..n, where
-- n is the number of accounts stored in wallet)
getAmountByIndex :: WorkMode m => A.RSCoinUserState -> Int -> m C.CoinsMap
getAmountByIndex st idx = do
    void $ updateBlockchain st False
    genAddr <- (^. C.genesisAddress) <$> getNodeContext
    addr <- flip atMay idx <$> query' st (A.GetOwnedDefaultAddresses genAddr)
    maybe
        (throwM $
         InputProcessingError "invalid index was given to getAmountByIndex")
        (getAmount st)
        addr

-- | Returns list of public addresses available
getAllPublicAddresses :: WorkMode m => A.RSCoinUserState -> m [C.Address]
getAllPublicAddresses st = do
    genAddr <- (^. C.genesisAddress) <$> getNodeContext
    query' st $ A.GetOwnedDefaultAddresses genAddr

genesisAddressIndex :: WorkMode m => A.RSCoinUserState -> m (Maybe Word)
genesisAddressIndex st = do
    addrList <- getAllPublicAddresses st
    genAddr <- (^. C.genesisAddress) <$> getNodeContext
    return $ fromIntegral <$> elemIndex genAddr addrList

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
addUserAddress
    :: MonadIO m
    => A.RSCoinUserState
    -> C.PublicKey
    -> Maybe C.SecretKey
    -> m ()
addUserAddress st pk skMaybe =
    update' st $ A.AddAddress (C.Address pk) skMaybe M.empty

-- | Same as addAddress, but queries blockchain automatically and
-- queries transactions that affect this address
importAddress
    :: WorkMode m
    => A.RSCoinUserState
    -> (Maybe C.SecretKey, C.PublicKey)
    -> Int
    -> m ()
importAddress st (skMaybe,pk) fromH = do
    whenJust skMaybe $ \sk ->
        unless (C.checkKeyPair (sk,pk)) $
            commitError "The provided pair doesn't match thus can't be used"
    ourSk <- query' st $ A.GetSecretKey $ C.Address pk
    case ourSk of
        Nothing -> return () -- it's ok
        Just Nothing | isJust skMaybe ->
            C.logInfo $ "The address doesn't have secret key, " <>
                        "adding this sk with re-query"
        Just Nothing ->
            commitError "This address doesn't have sk, won't update without provided"
        Just (Just _) -> commitError "The address is already imported"
    when (fromH < 0) $ commitError $
            sformat ("Height 'from' " % int % " must be positive!") fromH
    walletHeight <- query' st A.GetLastBlockId
    when (fromH > walletHeight) $
        let formatPattern =
                "fromH must be exactly within [0," % int % "]," %
                " where " % int %
                " is current wallet's top known blockchain height."
        in commitError $ sformat formatPattern walletHeight walletHeight
    let period = [fromH..walletHeight]
        perLength = walletHeight - fromH - 1
        delta = if perLength < 100
                then perLength
                else perLength `div` 10
        periodsLast = splitEvery delta period
    C.logInfo $ sformat
        ("Starting blockchain query process for blocks " % int % ".." % int) fromH walletHeight
    hblocks <- (period `zip`) . concat <$>
        mapM (\l -> C.getBlocksByHeight (head l) (last l)) periodsLast
    C.logInfo "Ended blockchain query process"
    update' st $ A.AddAddress newAddress skMaybe $ M.fromList hblocks
  where
    newAddress = C.Address pk
    splitEvery _ [] = []
    splitEvery n list = let (first,rest) = splitAt n list
                        in first : (splitEvery n rest)

-- | Deletes an address. Accepts an index in [0..length addresses)
deleteUserAddress :: (WorkMode m) => A.RSCoinUserState -> Int -> m ()
deleteUserAddress st ix = do
    checkAddressId st ix
    genAddr <- (^. C.genesisAddress) <$> getNodeContext
    addresses <- query' st $ A.GetOwnedAddresses genAddr
    update' st $ A.DeleteAddress $ addresses !! ix

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
    do genAddr <- (^. C.genesisAddress) <$> getNodeContext
       addrs <- query' st $ A.GetOwnedDefaultAddresses genAddr
       (indicesWithCoins :: [(Word, C.Coin)]) <-
           filter (not . C.isNegativeCoin . snd) <$>
           mapM
               (\i ->
                     (fromIntegral i + 1, ) . I.findWithDefault 0 0 <$>
                     getAmountByIndex st i)
               [0 .. length addrs - 1]
       let totalAmount = C.sumCoin $ map snd indicesWithCoins
       when (amount > totalAmount) $
           throwM $
           InputProcessingError $
           sformat
               ("Tried to form transaction with amount (" % build %
                ") greater than available (" % build % ").")
               amount totalAmount
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
       C.logInfo $
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
    C.logInfo $
        sformat
            ("Form a transaction from " % build % ", to " % build % ", amount " % build)
            (listBuilderJSONIndent 2 $ map
                  (\(a,b) ->
                        pairBuilder (a, listBuilderJSON b))
                  tdInputs)
            tdOutputAddress
            (listBuilderJSONIndent 2 $ tdOutputCoins)
    -- If there are multiple
    let tdInputsMerged :: [TransactionInput]
        tdInputsMerged = map (foldr1 (\(a,b) (_,d) -> (a, b++d))) $
                         groupBy ((==) `on` fst) $
                         sortOn fst tdInputs
    unless (all C.isPositiveCoin $ concatMap snd tdInputsMerged) $
        commitError $
        sformat
            ("All input values should be positive, but encountered " % build %
             ", that's not.") $
        head $ filter (not . C.isPositiveCoin) $ concatMap snd tdInputsMerged
    forM_ (map fst tdInputsMerged) $ checkAddressId st
    genAddr <- (^. C.genesisAddress) <$> getNodeContext
    accounts <- query' st $ A.GetOwnedAddresses genAddr
    let accInputs :: [(C.Address, I.IntMap C.Coin)]
        accInputs =
            map ((accounts `genericIndex`) *** C.coinsToMap) tdInputsMerged
        hasEnoughFunds :: (C.Address, C.CoinsMap) -> m Bool
        hasEnoughFunds (acc,coinsMap) = do
            amountMap <- getAmountNoUpdate st acc
            let sufficient =
                    all
                        (\col0 ->
                              let weHave = I.findWithDefault 0 col0 amountMap
                                  weSpend = I.findWithDefault 0 col0 coinsMap
                              in weHave >= weSpend)
                        (I.keys coinsMap)
            return sufficient
    overSpentAccounts <- filterM (fmap not . hasEnoughFunds) accInputs
    unless (null overSpentAccounts) $
        commitError $
        (if length overSpentAccounts > 1
             then "At least the"
             else "The") <>
        sformat
            (" following account doesn't have enough coins: " % build)
            (sel1 $ head overSpentAccounts)
    txPieces <-
        mapM
            (uncurry (submitTransactionMapper st tdOutputCoins tdOutputAddress))
            accInputs
    when (any isNothing txPieces) $
        commitError "Couldn't form transaction. Not enough funds."
    let (addrPairList,inputAddrids,outputs) =
            foldl1' pair3merge $ map fromJust txPieces
        outTr =
            C.Transaction
            { txInputs = inputAddrids
            , txOutputs = outputs ++ map (tdOutputAddress, ) tdOutputCoins
            }
    signatures <-
            M.fromList <$>
            mapM
                (\(addrid',address') -> do
                      (pK, sKMaybe) <-
                          query' st (A.FindUserAddress address')
                      when (isNothing sKMaybe) $ commitError $
                          sformat ("The correspondent account " % build %
                                   " doesn't have a secret key attached in" %
                                   " the wallet. Its related public component " %
                                   build) address' pK
                      return (addrid', (pK, C.sign (fromJust sKMaybe) outTr)))
                addrPairList
    when (not (null tdOutputCoins) && not (C.validateSum outTr)) $
        commitError $
        sformat ("Your transaction doesn't pass validity check: " % build) outTr
    when (null tdOutputCoins && not (C.validateSum outTr)) $
        commitError $
        sformat
            ("Our code is broken and our auto-generated transaction is invalid: " % build)
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
    :: WorkMode m
    => A.RSCoinUserState
    -> [C.Coin]
    -> C.Address
    -> C.Address
    -> C.CoinsMap
    -> m (Maybe ([(C.AddrId, C.Address)], [C.AddrId], [(C.Address, C.Coin)]))
submitTransactionMapper st outputCoin outputAddr address requestedCoins = do
    genAddr <- (^. C.genesisAddress) <$> getNodeContext
    (addrids :: [C.AddrId]) <- query' st (A.GetOwnedAddrIds genAddr address)
    let
        -- Pairs of chosen addrids and change for each color
        chosenMap0
            :: Maybe [([C.AddrId], C.Coin)]
        chosenMap0 = I.elems <$> C.chooseAddresses addrids requestedCoins
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
        C.logWarning $
            sformat
                ("Failed to send transaction (" % build % "), retries left: " % int)
                msg (tries - 1)
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
    C.logInfo $
        sformat
            ("Sending transaction: " % build % ", period id is " % int)
            tx
            periodId
    when (walletHeight /= lastAppliedBlock) $
        throwM $
        WalletSyncError $
        sformat
            ("Wallet isn't updated (lastBlockHeight " % int %
             " when blockchain's last block is " % int %").")
            walletHeight lastAppliedBlock
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
    C.logInfo $
        if willWeSend
        then "Successfully sent a transaction!"
        else "Somebody has sent a transaction, won't do it. Maybe retry later."

isRetriableException :: SomeException -> Bool
isRetriableException e
    | Just (_ :: UserLogicError) <- fromException e = True
    | isWalletSyncError e = True
    | otherwise = False

-- | Get list of all allocation address in which user participates.
retrieveAllocationsList
    :: forall m .
       WorkMode m
    => A.RSCoinUserState
    -> m ()  -- [(C.MSAddress, C.AllocationInfo)]
retrieveAllocationsList st = do
    -- @TODO: only first address as party is supported now
    genAddr         <- (^. C.genesisAddress) <$> getNodeContext
    fstUserAddress  <- head <$> query' st (A.GetOwnedAddresses genAddr)
    userAllocInfos  <- C.queryNotaryMyMSAllocations $ C.UserAlloc  fstUserAddress
    trustAllocInfos <- C.queryNotaryMyMSAllocations $ C.TrustAlloc fstUserAddress
    let allInfos = userAllocInfos ++ trustAllocInfos
    update' st $ A.UpdateAllocationStrategies $ M.fromList allInfos
    -- return allInfos
