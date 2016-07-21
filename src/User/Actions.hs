{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Module that provides description of what user node can do and
-- functions that runs chosen action.

module Actions
       ( processCommand
       , initializeStorage
       ) where

import           Control.Exception       (SomeException)
import           Control.Monad           (forM_, unless, void, when)
import           Control.Monad.Catch     (bracket, catch)
import           Control.Monad.Trans     (liftIO)

import qualified Data.Acid               as ACID
import           Data.Acid.Advanced      (query')
import qualified Data.ByteString.Base64  as B64
import           Data.Function           (on)
import           Data.List               (find, genericIndex, groupBy)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust, fromMaybe, isJust, mapMaybe)
import           Data.Monoid             ((<>))
import qualified Data.Set                as S
import qualified Data.Text               as T
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO

import           Formatting              (build, int, sformat, stext, (%))

import           Serokell.Util.Text      (format', formatSingle', show')

import qualified Graphics.UI.Gtk         as G
import           GUI.RSCoin.ErrorMessage (reportSimpleErrorNoWindow)
import           GUI.RSCoin.GUI          (startGUI)
import           GUI.RSCoin.GUIAcid      (emptyGUIAcid)

import qualified RSCoin.Core             as C
import           RSCoin.Core.Strategy    (AllocationAddress (..),
                                          AllocationInfo (..),
                                          AllocationStrategy (..),
                                          PartyAddress (..))
import           RSCoin.Timed            (WorkMode, for, ms, wait)
import qualified RSCoin.User             as U
import           RSCoin.User.Error       (eWrap)
import           RSCoin.User.Operations  (TransactionData (..),
                                          getAllPublicAddresses,
                                          getAmountNoUpdate, importAddress,
                                          submitTransactionRetry,
                                          updateBlockchain)
import qualified UserOptions             as O


initializeStorage
    :: forall (m :: * -> *).
       (WorkMode m)
    => U.RSCoinUserState
    -> O.UserOptions
    -> m ()
initializeStorage st O.UserOptions{..} =
    U.initState st addressesNum $ bankKeyPath isBankMode bankModePath
  where
    bankKeyPath True p = Just p
    bankKeyPath False _ = Nothing

-- | Processes command line user command
processCommand
    :: WorkMode m
    => U.RSCoinUserState -> O.UserCommand -> O.UserOptions -> m ()
processCommand st O.ListAddresses _ =
    eWrap $
    do res <- updateBlockchain st False
       unless res $ C.logInfo C.userLoggerName "Successfully updated blockchain."
       addresses <- query' st U.GetOwnedAddresses
       (wallets :: [(C.PublicKey, C.TxStrategy, [C.Coin])]) <-
           mapM (\addr -> do
                      coins <- C.coinsToList <$> getAmountNoUpdate st addr
                      strategy <- query' st $ U.GetAddressStrategy addr
                      return ( C.getAddress addr
                             , fromMaybe C.DefaultStrategy strategy
                             , coins))
                addresses
       liftIO $
           do TIO.putStrLn "Here's the list of your accounts:"
              TIO.putStrLn
                  "# | Public ID                                    | Amount"
              mapM_ formatAddressEntry $ ([(1 :: Integer) ..] `zip` wallets)
  where
    spaces = "                                                   "
    formatAddressEntry :: (Integer, (C.PublicKey, C.TxStrategy, [C.Coin])) -> IO ()
    formatAddressEntry (i, (key, strategy, coins)) = do
       TIO.putStr $ format' "{}.  {} : " (i, key)
       when (null coins) $ putStrLn "empty"
       unless (null coins) $ TIO.putStrLn $ show' $ head coins
       unless (length coins < 2) $
           forM_ (tail coins)
                 (TIO.putStrLn . formatSingle' (spaces <> "{}"))
       case strategy of
           C.DefaultStrategy -> return ()
           C.MOfNStrategy m allowed -> do
               TIO.putStrLn $ format'
                    "    This is a multisig address ({}/{}) controlled by keys: "
                    (m, length allowed)
               forM_ allowed $ \allowedAddr -> do
                   addresses <- query' st U.GetOwnedAddresses
                   TIO.putStrLn $ formatSingle'
                           (if allowedAddr `elem` addresses
                            then "    * {} owned by you"
                            else "    * {}")
                           allowedAddr
processCommand st (O.FormTransaction inputs outputAddrStr outputCoins cache) _ =
    eWrap $
    do let outputAddr = C.Address <$> C.constructPublicKey outputAddrStr
           inputs' = map (foldr1 (\(a,b) (_,d) -> (a, b ++ d))) $
                     groupBy ((==) `on` snd) $
                     map (\(idx,o,c) -> (idx - 1, [C.Coin c (toRational o)]))
                     inputs
           outputs' = map (\(amount,color) -> C.Coin color (toRational amount))
                          outputCoins
           td = TransactionData
                { tdInputs = inputs'
                , tdOutputAddress = fromJust outputAddr
                , tdOutputCoins = outputs'
                }
       unless (isJust outputAddr) $
           U.commitError $ "Provided key can't be exported: " <> outputAddrStr
       void $ submitTransactionRetry 2 st cache td
processCommand st O.UpdateBlockchain _ =
    eWrap $
    do res <- updateBlockchain st True
       C.logInfo C.userLoggerName $
           if res
               then "Blockchain is updated already."
               else "Successfully updated blockchain."
processCommand st (O.Dump command) _ = eWrap $ dumpCommand st command
processCommand _ (O.SignSeed seedB64 mPath) _ = liftIO $ do
    sk <- maybe (pure C.attainSecretKey) C.readSecretKey mPath
    (seedPk, _) <- case B64.decode $ encodeUtf8 seedB64 of
              Left _ -> fail "Wrong seed supplied (base64 decoding failed)"
              Right s -> maybe (fail "Failed to derive keypair from seed") pure $ C.deterministicKeyGen s
    liftIO $ TIO.putStrLn $
       sformat ("Seed Pk: " % build) seedPk
    let (pk, sig) = (C.derivePublicKey sk, C.sign sk seedPk)
    liftIO $ TIO.putStrLn $
       sformat ("AttPk: " % build % ", AttSig: " % build % ", verifyChain: " % build) pk sig (C.verifyChain pk [(sig, seedPk)])
processCommand st (O.AddMultisigAddress m textUAddrs textTAddrs mMSAddress) _ = do
    when (null textUAddrs && null textTAddrs) $
        U.commitError "Can't create multisig with empty addrs list"

    userAddrs  <- map C.UserAlloc  <$> parseTextAddresses textUAddrs
    trustAddrs <- map C.TrustAlloc <$> parseTextAddresses textTAddrs
    let partiesAddrs = userAddrs ++ trustAddrs
    when (m > length partiesAddrs) $
        U.commitError "Parameter m should be less than length of list"

    msPublicKey <- maybe (snd <$> liftIO C.keyGen) return (mMSAddress >>= C.constructPublicKey)
    (userAddress, userSk) <- head <$> query' st U.GetUserAddresses
    let msAddr    = C.Address msPublicKey
    let partyAddr = C.UserParty userAddress
    let msStrat   = C.AllocationStrategy m $ S.fromList partiesAddrs
    let userSignature = C.sign userSk (msAddr, msStrat)
    let certChain     = U.createCertificateChain $ C.getAddress userAddress
    C.allocateMultisignatureAddress
        msAddr
        partyAddr
        msStrat
        userSignature
        certChain
    liftIO $ TIO.putStrLn $
       sformat ("Your new address will be added in the next block: " % build) msPublicKey
  where
    parseTextAddresses :: WorkMode m => [T.Text] -> m [C.Address]
    parseTextAddresses textAddrs = do
        let partiesAddrs = mapMaybe (fmap C.Address . C.constructPublicKey) textAddrs
        when (length partiesAddrs /= length textAddrs) $ do
            let parsed = T.unlines (map show' partiesAddrs)
            U.commitError $
                sformat ("Some addresses were not parsed, parsed only those: " % stext) parsed
        return partiesAddrs
processCommand st (O.ConfirmAllocation i) _ = eWrap $ do
    when (i <= 0) $ U.commitError $
        sformat ("Index i should be greater than 0 but given: " % int) i

    (msAddr, C.AllocationInfo{..}) <- query' st $ U.GetAllocationByIndex (i - 1)
    (userAddress, userSk) <- head <$> query' st U.GetUserAddresses

    let Just party = find ((== userAddress) . _address) $ _allParties _allocationStrategy
    partyAddr <- case party of
        C.TrustAlloc {} -> do
            (_, newPk) <- liftIO C.keyGen
            return C.TrustParty { generatedAddress = C.Address newPk
                                , publicAddress    = userAddress }
        C.UserAlloc  {} -> return $ C.UserParty userAddress

    let partySignature = C.sign userSk (msAddr, _allocationStrategy)
    C.allocateMultisignatureAddress
        msAddr
        partyAddr
        _allocationStrategy
        partySignature
        []
    liftIO $ TIO.putStrLn "Address allocation successfully confirmed!"
processCommand st O.ListAllocations _ = eWrap $ do
    -- update local cache
    U.retrieveAllocationsList st
    msAddrsList <- query' st U.GetAllocationStrategies
    liftIO $ TIO.putStrLn $ T.pack $ show msAddrsList
    msigAddrsList <- (`zip` [(1::Int)..]) . M.assocs <$> query' st U.GetAllocationStrategies
    when (null msigAddrsList) $
        liftIO $ putStrLn "Allocation address list is empty"
    forM_ msigAddrsList $ \((addr,allocStrat), i) -> do
        let numLength = length $ show i
        let pattern = "{}. {}\n  {}" <> (mconcat $ replicate numLength " ")
        liftIO $ TIO.putStrLn $ format' pattern (i, addr, allocStrat)
processCommand st (O.ImportAddress skPath pkPath heightFrom heightTo) _ = do
    liftIO $ TIO.putStrLn "Reading sk/pk from files..."
    sk <- liftIO $ C.readSecretKey skPath
    pk <- liftIO $ C.readPublicKey pkPath
    when (not $ C.checkKeyPair (sk,pk)) $
        U.commitError "The provided pair doesn't match thus can't be used"
    allAddrs <- getAllPublicAddresses st
    when ((C.Address pk) `elem` allAddrs) $
        U.commitError "Address  is already imported into wallet"
    importAddress st (sk,pk) heightFrom heightTo
processCommand st O.StartGUI opts@O.UserOptions{..} = do
    initialized <- U.isInitialized st
    unless initialized $ liftIO G.initGUI >> initLoop
    liftIO $ bracket
        (ACID.openLocalStateFrom guidbPath emptyGUIAcid)
        (\cs -> do ACID.createCheckpoint cs
                   ACID.closeAcidState cs)
        (\cs -> startGUI st cs)
  where
    initLoop =
        initializeStorage st opts `catch`
        (\(e :: SomeException) ->
              do liftIO $
                     reportSimpleErrorNoWindow $
                     "Couldn't initialize rscoin. Check connection, close this " ++
                     "dialog and we'll try again. Error: "
                     ++ show e
                 wait $ for 500 ms
                 initLoop)

dumpCommand
    :: WorkMode m
    => U.RSCoinUserState -> O.DumpCommand -> m ()
dumpCommand _ O.DumpMintettes = void C.getMintettes
dumpCommand _ O.DumpAddresses = void C.getAddresses
dumpCommand _ O.DumpPeriod = void C.getBlockchainHeight
dumpCommand _ (O.DumpHBlocks from to) = void $ C.getBlocks from to
dumpCommand _ (O.DumpHBlock pId) = void $ C.getBlockByHeight pId
dumpCommand _ (O.DumpLogs mId from to) = void $ C.getLogs mId from to
dumpCommand _ (O.DumpMintetteUtxo mId) = void $ C.getMintetteUtxo mId
dumpCommand _ (O.DumpMintetteBlocks mId pId) =
    void $ C.getMintetteBlocks mId pId
dumpCommand _ (O.DumpMintetteLogs mId pId) = void $ C.getMintetteLogs mId pId
dumpCommand st (O.DumpAddress idx) =
    liftIO . TIO.putStrLn . show' . (`genericIndex` (idx - 1)) =<<
    query' st U.GetOwnedAddresses
