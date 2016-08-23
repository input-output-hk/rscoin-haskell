{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Module that provides description of what user node can do and
-- functions that runs chosen action.

module Actions
       ( processCommand
       , initializeStorage
       ) where

import           Control.Applicative     (liftA2)
import           Control.Lens            ((^.))
import           Control.Monad           (forM_, join, unless, void, when)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Trans     (liftIO)

import           Data.Aeson              (decode, encode)
import           Data.Bifunctor          (bimap, first, second)
import qualified Data.ByteString.Lazy    as BS
import           Data.Char               (isSpace)
import           Data.Function           (on)
import qualified Data.HashSet            as HS
import           Data.Int                (Int64)
import           Data.List               (genericIndex, groupBy, intersperse)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust, fromMaybe, isJust, mapMaybe)
import           Data.Monoid             ((<>))
import qualified Data.Set                as S hiding (Set)
import qualified Data.Text               as T
import qualified Data.Text.Buildable     as B (Buildable (build))
import qualified Data.Text.IO            as TIO
import           Formatting              (build, int, sformat, stext, string,
                                          (%))

import           Serokell.Util.Text      (listBuilderJSONIndent, pairBuilder,
                                          show')

#if GtkGui
import           Control.Exception       (SomeException)
import           Control.Monad.Catch     (bracket, catch)
import qualified Data.Acid               as ACID
import qualified Graphics.UI.Gtk         as G
import           GUI.RSCoin.ErrorMessage (reportSimpleErrorNoWindow)
import           GUI.RSCoin.GUI          (startGUI)
import           GUI.RSCoin.GUIAcid      (emptyGUIAcid)
import           RSCoin.Timed            (for, ms, wait)
#endif

import qualified RSCoin.Core             as C
import           RSCoin.Core.Aeson       ()
import           RSCoin.Core.Strategy    (AllocationAddress (..),
                                          PartyAddress (..))
import           RSCoin.Timed            (WorkMode, getNodeContext)
import qualified RSCoin.User             as U
import           RSCoin.User.Error       (eWrap)
import           RSCoin.User.Operations  (TransactionData (..), checkAddressId,
                                          constructTransaction,
                                          deleteUserAddress,
                                          getAllPublicAddresses,
                                          getAmountNoUpdate,
                                          getEmptySignatureBundle,
                                          importAddress, submitTransactionRetry,
                                          updateBlockchain)
import qualified UserOptions             as O


initializeStorage
    :: forall (m :: * -> *).
       (WorkMode m)
    => U.UserState
    -> O.UserOptions
    -> m ()
initializeStorage st O.UserOptions{..} =
    U.initState st addressesNum $ bankKeyPath isBankMode bankModePath
  where
    bankKeyPath True p  = Just p
    bankKeyPath False _ = Nothing

processCommand
    :: (MonadIO m, WorkMode m)
    => U.UserState -> O.UserCommand -> O.UserOptions -> m ()
#if GtkGui
processCommand st O.StartGUI opts = processStartGUI st opts
#endif
processCommand st command _       = processCommandNoOpts st command

-- | Processes command line user command
processCommandNoOpts
    :: (MonadIO m, WorkMode m)
    => U.UserState -> O.UserCommand -> m ()
processCommandNoOpts st O.ListAddresses =
    processListAddresses st
processCommandNoOpts st (O.FormTransaction inp out outC) =
    processFormTransaction st inp out outC
processCommandNoOpts st O.UpdateBlockchain =
    processUpdateBlockchain st
processCommandNoOpts st (O.CreateMultisigAddress n usrAddr trustAddr masterPk sig) =
    processMultisigAddress st n usrAddr trustAddr masterPk sig
processCommandNoOpts st (O.ListPendingTransactions) =
    processListPendingTxs st
processCommandNoOpts st (O.SendPendingTransaction index) =
    processSendPendingTx st index
processCommandNoOpts st (O.ConfirmAllocation i mHot masterPk sig) =
    processConfirmAllocation st i mHot masterPk sig
processCommandNoOpts st (O.ListAllocations mTrustAddrText) =
    processListAllocation st mTrustAddrText False
processCommandNoOpts st (O.ListAllocationsBlacklist mTrustAddrText) =
    processListAllocation st mTrustAddrText True
processCommandNoOpts st (O.BlacklistAllocation ix) =
    processBlackWhiteListing st True ix
processCommandNoOpts st (O.WhitelistAllocation ix) =
    processBlackWhiteListing st False ix
processCommandNoOpts st (O.ColdFormTransaction inp out outC path) =
    processColdFormTransaction st inp out outC path
processCommandNoOpts st (O.ColdSignTransaction path) =
    processColdSignTransaction st path
processCommandNoOpts st (O.ColdSendTransaction path) =
    processColdSendTransaction st path
processCommandNoOpts st (O.ImportAddress skPathMaybe pkPath heightFrom) =
    processImportAddress st skPathMaybe pkPath heightFrom
processCommandNoOpts st (O.ExportAddress addrId filepath) =
    processExportAddress st addrId filepath
processCommandNoOpts st (O.DeleteAddress ix force) =
    processDeleteAddress st ix force
processCommandNoOpts st (O.Dump command) =
    eWrap $ dumpCommand st command

processListAddresses
    :: (MonadIO m, WorkMode m)
    => U.UserState -> m ()
processListAddresses st =
    eWrap $
    do res <- updateBlockchain st False
       unless res $ C.logInfo "Successfully updated blockchain."
       genAddr <- (^. C.genesisAddress) <$> getNodeContext
       addresses <- U.query st $ U.GetOwnedAddresses genAddr
       (wallets :: [(C.PublicKey, C.TxStrategy, [C.Coin], Bool)]) <-
           mapM (\addr -> do
                      coins <- C.coinsToList <$> getAmountNoUpdate st addr
                      hasSecret <- isJust . snd <$> U.query st (U.FindUserAddress addr)
                      strategy <- U.query st $ U.GetAddressStrategy addr
                      return ( C.getAddress addr
                             , fromMaybe C.DefaultStrategy strategy
                             , coins
                             , hasSecret))
                addresses
       liftIO $ do
           TIO.putStrLn "Here's the list of your accounts:"
           TIO.putStrLn
               "# | Public ID                                    | Amount"
       mapM_ formatAddressEntry ([(1 :: Integer) ..] `zip` wallets)
  where
    spaces = "                                                   "
    formatAddressEntry
        :: (WorkMode m)
        => (Integer, (C.PublicKey, C.TxStrategy, [C.Coin], Bool))
        -> m ()
    formatAddressEntry (i, (key, strategy, coins, hasSecret)) = do
        liftIO $ do
            TIO.putStr $ sformat (int%".  "%build%" : ") i key
            when (null coins) $ putStrLn "empty"
            unless (null coins) $ TIO.putStrLn $ show' $ head coins
            unless (length coins < 2) $
                forM_ (tail coins)
                      (TIO.putStrLn . sformat (spaces % build))
            unless hasSecret $ TIO.putStrLn "    (!) This address doesn't have secret key"
        case strategy of
            C.DefaultStrategy -> return ()
            C.MOfNStrategy m allowed -> do
                genAddr <- (^. C.genesisAddress) <$> getNodeContext
                liftIO $ do
                    TIO.putStrLn $ sformat
                         ("    This is a multisig address ("%int%"/"%int%") controlled by keys: ")
                         m (length allowed)
                    forM_ allowed $ \allowedAddr -> do
                        addresses <- U.query st $ U.GetOwnedAddresses genAddr
                        TIO.putStrLn $ sformat
                            (if allowedAddr `elem` addresses
                             then "    * "%build%" owned by you"
                             else "    * "%build)
                            allowedAddr

formTransactionPayload
    :: (WorkMode m)
    => [(Word, Int64, Int)] -> T.Text -> [(Int64, Int)] -> m TransactionData
formTransactionPayload inputs outputAddrStr outputCoins = eWrap $ do
    unless (isJust outputAddr) $
        U.commitError $
        "Provided key can't be read/imported (check format): " <> outputAddrStr
    return td
  where
    outputAddr = C.Address <$> C.constructPublicKey outputAddrStr
    inputs' =
        map (foldr1 (\(a,b) (_,d) -> (a, b ++ d))) $
        groupBy ((==) `on` snd) $
        map (\(idx,o,c) -> (idx - 1, [C.Coin (C.Color c) (C.CoinAmount $ toRational o)]))
        inputs
    outputs' =
        map (uncurry (flip C.Coin) . bimap (C.CoinAmount . toRational) C.Color)
            outputCoins
    td = TransactionData
         { tdInputs = inputs'
         , tdOutputAddress = fromJust outputAddr
         , tdOutputCoins = outputs'
         }

processFormTransaction
    :: (MonadIO m, WorkMode m)
    => U.UserState -> [(Word, Int64, Int)] -> T.Text -> [(Int64, Int)] -> m ()
processFormTransaction st inputs outputAddrStr outputCoins =
    eWrap $
    do td <- formTransactionPayload inputs outputAddrStr outputCoins
       tx <- submitTransactionRetry 2 st Nothing td
       C.logInfo $
           sformat ("Successfully submitted transaction with hash: " % build) $
           C.hash tx

processMultisigAddress
    :: (MonadIO m, WorkMode m)
    => U.UserState
    -> Int
    -> [T.Text]
    -> [T.Text]
    -> Maybe T.Text
    -> Maybe T.Text
    -> m ()
processMultisigAddress st m textUAddrs textTAddrs mMasterPkText mMasterSlaveSigText = do
    when (null textUAddrs && null textTAddrs) $
        U.commitError "Can't create multisig with empty addrs list"

    userAddrs       <- map UserAlloc  <$> parseTextAddresses textUAddrs
    trustAddrs      <- map TrustAlloc <$> parseTextAddresses textTAddrs
    let partiesAddrs = userAddrs ++ trustAddrs
    let partySet     = HS.fromList partiesAddrs
    when (m > HS.size partySet) $
        U.commitError "Parameter m should be less than length of list"

    (userPartyAddr, userSk) <- U.findPartyAddress st partySet
    msPublicKey             <- snd  <$> liftIO C.keyGen
    let msAddr               = C.Address msPublicKey
    let partyAddr            = C.UserParty userPartyAddr
    let msStrat              = C.AllocationStrategy m partySet
    let userSignature        = C.sign userSk (msAddr, msStrat)

    -- @TODO: replace with Either and liftA2
    -- @TODO: we lose errors :(
    let mMasterPk       = join $ C.constructPublicKey <$> mMasterPkText
    let mMasterSlaveSig = join $ C.constructSignature <$> mMasterSlaveSigText
    let mMasterCheck    = liftA2 (,) mMasterPk mMasterSlaveSig

    C.allocateMultisignatureAddress
        msAddr
        partyAddr
        msStrat
        userSignature
        mMasterCheck

    C.logInfo $ sformat
        ("Your new address will be added in the next block after it's confirmed: " % build)
        msPublicKey
  where
    parseTextAddresses
        :: WorkMode m
        => [T.Text] -> m [C.Address]
    parseTextAddresses textAddrs = do
        let partiesAddrs = mapMaybe (fmap C.Address . C.constructPublicKey) textAddrs
        when (length partiesAddrs /= length textAddrs) $
            do let parsed = T.unlines (map show' partiesAddrs)
               U.commitError $
                   sformat
                       ("Some addresses were not parsed, parsed only those: " %
                        stext)
                       parsed
        return partiesAddrs

processListPendingTxs :: (MonadIO m, WorkMode m) => U.UserState -> m ()
processListPendingTxs st = do
    addrs <- getAllPublicAddresses st
    C.logInfo "Querying notary to update the list of pending transactions"
    txs0 <- C.pollPendingTransactions addrs
    U.update st $ U.UpdatePendingTxs $ S.fromList txs0
    txs <- U.query st U.GetPendingTxs
    if null txs
    then do
        C.logInfo "Successfully updated pending txs list, but it's empty"
        liftIO $ TIO.putStrLn "No pending transactions"
    else do
        C.logInfo "Successfully updated pending txs list"
        forM_ ([(1::Integer)..] `zip` txs) $ \(i,transaction) -> do
            builder <- altTxBuilder transaction
            liftIO $ TIO.putStrLn $ sformat (int % ". " % build) i builder
  where
    altTxBuilder tx@C.Transaction{..} = do
        resolved <-
            mapM (\addrid -> (addrid,) <$>
                     U.query st (U.ResolveAddressLocally addrid))
                 txInputs
        let mapFoo (addrid,Nothing) = B.build addrid
            mapFoo (addrid,Just addr) =
                B.build addrid <> " that's owned by addr " <> B.build addr
            mappedResolved = map mapFoo resolved
            builtInputs = mconcat $ intersperse ",\n  " mappedResolved
        return $ mconcat ["Transaction{\ninputs:"
                         , builtInputs
                         , "\noutputs:"
                         , listBuilderJSONIndent 2 $ map pairBuilder txOutputs
                         , "\nhash: "
                         , B.build $ C.hash tx
                         , "\n}"
                         ]

processSendPendingTx
    :: (MonadIO m, WorkMode m)
    => U.UserState -> Int -> m ()
processSendPendingTx st ix0 = do
    txs <- U.query st U.GetPendingTxs
    let l = length txs
    when (null txs) $ U.commitError "No transactions are currently pending."
    when (ix < 0 || ix >= l) $
        U.commitError $
        sformat
            ("Index is out of range [1," % int % "], where " % int %
             " is total number of pending txs at the moment") l l
    let tx = txs !! ix
    emptyBundle <- U.getEmptySignatureBundle st tx
    signatures <- U.signTransactionLocally st tx emptyBundle
    C.logInfo "Collecting signatures & sending"
    U.sendTransactionRetry 3 st Nothing tx signatures
  where
    ix = ix0 - 1

processUpdateBlockchain
    :: (MonadIO m, WorkMode m)
    => U.UserState -> m ()
processUpdateBlockchain st =
    eWrap $
    do res <- updateBlockchain st True
       C.logInfo $
           if res
               then "Blockchain is updated already."
               else "Successfully updated blockchain."

processConfirmAllocation
    :: (MonadIO m, WorkMode m)
    => U.UserState
    -> Int
    -> Maybe String
    -> Maybe T.Text
    -> Maybe T.Text
    -> m ()
processConfirmAllocation st i mHot mMasterPkText mMasterSlaveSigText =
    eWrap $
    do when (i <= 0) $  -- Crazy indentation ;(
           U.commitError $
           sformat ("Index i should be greater than 0 but given: " % int) i

       strategiesSize <- M.size <$> U.query st U.GetAllocationStrategies
       when (strategiesSize == 0) $
           U.commitError "No allocation strategies are saved. Execute list-alloc again"
       when (strategiesSize < i) $
           U.commitError $ sformat
           ("Only " % int % " strategies are available, index " %
            int % " is out of range.") strategiesSize i

       (msAddr, allocInfo)  <- U.query st $ U.GetAllocationByIndex (i - 1)
       let submittedStrategy = allocInfo         ^. C.allocationStrategy
       let strategyParties   = submittedStrategy ^. C.allParties
       (partyAddr, slaveSk) <- case mHot of
           Just (read -> (hotSkPath, partyPkStr)) -> do
               U.verifyTrustEntry st strategyParties

               hotSk <- liftIO $ C.readSecretKey hotSkPath
               let party = C.Address $ fromMaybe
                               (error "Not valid hot partyPk!")
                               (C.constructPublicKey $ T.pack partyPkStr)
               let partyAddr = C.TrustParty { partyAddress = party
                                            , hotTrustKey  = C.derivePublicKey hotSk
                                            }
               return (partyAddr, hotSk)
           Nothing -> first C.UserParty <$> U.findPartyAddress st strategyParties

       let partySignature  = C.sign slaveSk (msAddr, submittedStrategy)
       let mMasterPk       = join $ C.constructPublicKey <$> mMasterPkText
       let mMasterSlaveSig = join $ C.constructSignature <$> mMasterSlaveSigText
       let mMasterCheck    = liftA2 (,) mMasterPk mMasterSlaveSig

       C.allocateMultisignatureAddress
           msAddr
           partyAddr
           submittedStrategy
           partySignature
           mMasterCheck

       C.logInfo "Address allocation successfully confirmed!"

-- | Boolean flags stands for "show blacklist?". Default -- false,
-- show whitelist.
processListAllocation
    :: (MonadIO m, WorkMode m)
    => U.UserState
    -> Maybe T.Text
    -> Bool
    -> m ()
processListAllocation st mTrustAddrText blacklist = eWrap $ do
    let trustAddr = C.Address <$> join (C.constructPublicKey <$> mTrustAddrText)
    U.retrieveAllocationsList st trustAddr

    msigAddrsList <-
        (`zip` [(1 :: Int) ..]) . M.assocs <$>
        (if blacklist
         then U.query st U.GetIgnoredAllocationStrategies
         else U.query st U.GetAllocationStrategies)
    if null msigAddrsList
    then liftIO $ putStrLn ("Allocation address " ++
            (if blacklist then "black" else "") ++ "list is empty")
    else do
        when blacklist $
            liftIO $ putStrLn "Here is a *blacklisted* strategy list:"
        forM_ msigAddrsList $
            \((addr,allocStrat),i) ->
                liftIO $ TIO.putStrLn $ sformat
                    (int % ". " % build % "\n" % build) i addr allocStrat

-- | Blacklists and whitelists allocations. Bool true mean
-- "blacklist". False -- "whitelist".
processBlackWhiteListing
    :: (MonadIO m, WorkMode m)
    => U.UserState -> Bool -> Int -> m ()
processBlackWhiteListing st blacklist ix =
    eWrap $
    do msaddrs <- M.keys <$>
           (if blacklist
            then U.query st U.GetAllocationStrategies
            else U.query st U.GetIgnoredAllocationStrategies)
       when (ix <= 0 || ix > length msaddrs) $
           U.commitError $
           sformat ("index " % int % " should be positive and no bigger than " %
                    int % " -- the size of " % listName) ix (length msaddrs)
       let msaddr = msaddrs !! (ix - 1)
       if blacklist
       then U.update st $ U.BlacklistAllocation msaddr
       else U.update st $ U.WhitelistAllocation msaddr
       C.logInfo successText
  where
    listName = if blacklist then "blacklist" else "alloc list"
    successText = if blacklist
                  then "Allocation was successfully blacklisted"
                  else "Allocation was successfully whitelisted back"

-- | Forms transaction, its related empty signature bundle and dumps
-- it to the file
processColdFormTransaction
    :: (MonadIO m, WorkMode m)
    => U.UserState
    -> [(Word, Int64, Int)]
    -> T.Text
    -> [(Int64, Int)]
    -> FilePath
    -> m ()
processColdFormTransaction st inputs outputAddrStr outputCoins path = eWrap $ do
    td <- formTransactionPayload inputs outputAddrStr outputCoins
    tx <- constructTransaction st td
    emptyBundle <- getEmptySignatureBundle st tx
    liftIO $ BS.writeFile path $ encode (tx, M.assocs emptyBundle)
    C.logInfo $ sformat
        ("Your transaction data has been written to the '" % string % "'") path

-- | Signs transaction from given file and writes output into new file with
-- @.signed@ extension with new signature bundle.
processColdSignTransaction
    :: WorkMode m
    => U.UserState
    -> FilePath
    -> m ()
processColdSignTransaction st bundlePath = eWrap $ do
    (tx, sigAssocs) :: (C.Transaction, [(C.AddrId, U.SignatureValue)]) <-
        liftIO $ (fromJust . decode) <$> BS.readFile bundlePath
    let sigBundle = M.fromList sigAssocs
    updatedSigBundle <- U.signTransactionLocally st tx sigBundle
--    updatedSigBundle <- forM sigAssocs $ \(addrId, (msAddr, C.MOfNStrategy m p, signatures)) -> do
--        (userAddr, userSk) <- U.findPartyAddress st $ HS.fromList $ map C.UserAlloc $ S.toList p
--        pure (addrId, (msAddr, C.MOfNStrategy m p, (userAddr, C.sign userSk tx) : signatures))
    -- @TODO: not efficient check
    if sigBundle == updatedSigBundle
    then C.logInfo "No transactions have been signed"
    else do
        liftIO $ BS.writeFile (bundlePath <> ".signed") $
            encode (tx, M.assocs updatedSigBundle)
        C.logInfo "Some transactions have been succesfully signed!"

processColdSendTransaction
    :: WorkMode m
    => U.UserState
    -> FilePath
    -> m ()
processColdSendTransaction st bundlePath = eWrap $ do
    (tx, sigAssocs) :: (C.Transaction, [(C.AddrId, U.SignatureValue)]) <-
        liftIO $ (fromJust . decode) <$> BS.readFile bundlePath
    let sigBundle = M.fromList sigAssocs
    U.sendTransactionRetry 2 st Nothing tx sigBundle

processImportAddress
    :: (MonadIO m, WorkMode m)
    => U.UserState
    -> Maybe FilePath
    -> FilePath
    -> Int
    -> m ()
processImportAddress st skPathMaybe pkPath heightFrom = do
    pk <- liftIO $ C.logInfo "Reading pk..." >> C.readPublicKey pkPath
    sk <- liftIO $ flip (maybe (return Nothing)) skPathMaybe $ \skPath ->
        C.logInfo "Reading sk..." >> Just <$> C.readSecretKey skPath
    importAddress st (sk,pk) heightFrom
    C.logInfo "Finished, your address successfully added"

processExportAddress
    :: (MonadIO m, WorkMode m)
    => U.UserState
    -> Int
    -> FilePath
    -> m ()
processExportAddress st ix0 filepath = do
    let ix = ix0 - 1
    checkAddressId st ix
    allAddresses <- getAllPublicAddresses st
    let addr = allAddresses !! ix
    strategy <- fromJust <$> U.query st (U.GetAddressStrategy addr)
    case strategy of
        C.DefaultStrategy -> do
            C.logInfo
                "The strategy of your address is default, dumping it to the file"
            (addr'@(C.getAddress -> pk),sk) <-
                second fromJust <$> U.query st (U.FindUserAddress addr)
            unless (addr' == addr) $
                C.logError $
                "Internal error, address found is not the same " <>
                "as requested one for default strategy"
            liftIO $ C.writeSecretKey filepath sk
            liftIO $ C.writePublicKey (filepath <> ".pub") pk
            C.logInfo $
                sformat
                    ("Dumped secret key into '" % string %
                     "', public into '" % string % ".pub'")
                    filepath
                    filepath
        C.MOfNStrategy m parties ->
            U.commitError $
            sformat
                ("This address is " % int % "/" % int %
                 " strategy address, export correspondent key instead. " %
                 "Correspondent m/n key are autoexported " %
                 "when you import their party.")
                m
                (S.size parties)

processDeleteAddress
    :: (MonadIO m, WorkMode m)
    => U.UserState -> Int -> Bool -> m ()
processDeleteAddress st ix0 force =
    eWrap $
    do C.logInfo $ sformat ("Deleting address #" % int) ix0
       checkAddressId st ix
       ourAddr <- (!! ix) <$> getAllPublicAddresses st
       dependent <- U.query st $ U.GetDependentAddresses ourAddr
       unless (null dependent) $ liftIO $ do
           TIO.putStrLn $ "These addresses depend on the requested one so they " <>
                          "will be removed as well:"
           forM_ dependent $ TIO.putStrLn . sformat ("  " % build)
       if force
       then proceedDeletion
       else askConfirmation ourAddr
  where
    print' = liftIO . TIO.putStrLn
    ix = ix0 - 1
    looksLikeYes s = let s' = T.toLower s in s' == "y" || s' == "yes"
    looksLikeNo s = let s' = T.toLower s in s' == "n" || s' == "no"
    proceedDeletion = do
       C.logInfo "Deleting address..."
       deleteUserAddress st ix
       C.logInfo "Address was successfully deleted"
    askConfirmation ourAddr = do
        print' $ sformat ("Are you sure you want to delete this address: " %
                     build % " ?") ourAddr
        print' "(y/n)"
        str <- liftIO $ T.dropAround isSpace <$> TIO.getLine
        if looksLikeYes str then proceedDeletion
        else if looksLikeNo str then print' "Deletion canceled"
        else do
            print' "Couldn't parse your answer. Y/N?"
            askConfirmation ourAddr

#if GtkGui
processStartGUI
    :: (MonadIO m, WorkMode m)
    -> U.UserState
    -> O.UserOptions
    -> m ()
processStartGUI st opts@O.UserOptions{..} = do
    initialized <- U.isInitialized st
    unless initialized $ liftIO G.initGUI >> initLoop
    liftIO $
        bracket
            (ACID.openLocalStateFrom guidbPath emptyGUIAcid)
            (\cs -> do
                 ACID.createCheckpoint cs
                 ACID.closeAcidState cs)
            (\cs ->
                  startGUI (Just configPath) st cs)
  where
    initLoop =
        initializeStorage st opts `catch`
        (\(e :: SomeException) -> do
             liftIO $
                 reportSimpleErrorNoWindow $
                 "Couldn't initialize rscoin. Check connection, close this " ++
                 "dialog and we'll try again. Error: " ++ show e
             wait $ for 500 ms
             initLoop)
#endif

dumpCommand
    :: WorkMode m
    => U.UserState -> O.DumpCommand -> m ()
dumpCommand _ O.DumpMintettes = void C.getMintettes
dumpCommand _ O.DumpAddresses = void C.getAddresses
dumpCommand _ O.DumpPeriod = void C.getBlockchainHeight
dumpCommand _ (O.DumpHBlocks from to) = void $ C.getBlocksByHeight from to
dumpCommand _ (O.DumpHBlock pId) = void $ C.getBlockByHeight pId
dumpCommand _ (O.DumpLogs mId from to) = void $ C.getLogs mId from to
dumpCommand _ (O.DumpMintetteUtxo mId) = void $ C.getMintetteUtxo mId
dumpCommand _ (O.DumpMintetteBlocks mId pId) =
    void $ C.getMintetteBlocks mId pId
dumpCommand _ (O.DumpMintetteLogs mId pId) = void $ C.getMintetteLogs mId pId
dumpCommand st (O.DumpAddress idx) =
    C.logInfo . show' . (`genericIndex` (idx - 1)) =<<
    (\ctx ->
          U.query st (U.GetOwnedAddresses (ctx ^. C.genesisAddress))) =<<
    getNodeContext
