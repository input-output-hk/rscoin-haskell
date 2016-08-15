{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Module that provides description of what user node can do and
-- functions that runs chosen action.

module Actions
       ( processCommand
       , initializeStorage
       ) where

import           Control.Lens           ((^.))
import           Control.Monad          (forM_, unless, void, when)
import           Control.Monad.Trans    (MonadIO, liftIO)
import           Data.Acid.Advanced     (query')
import           Data.Bifunctor         (bimap)
import qualified Data.ByteString.Base64 as B64
import           Data.Function          (on)
import           Data.List              (find, genericIndex, groupBy)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust, fromMaybe, isJust, mapMaybe)
import           Data.Monoid            ((<>))
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Text.IO           as TIO
import           Formatting             (build, int, sformat, stext, (%))

import           Serokell.Util.Text     (show')

import qualified RSCoin.Core            as C
import           RSCoin.Core.Strategy   (AllocationAddress (..),
                                         AllocationInfo (..),
                                         AllocationStrategy (..),
                                         PartyAddress (..))
import           RSCoin.Timed           (WorkMode, getNodeContext)
import qualified RSCoin.User            as U
import           RSCoin.User.Error      (eWrap)
import           RSCoin.User.Operations (TransactionData (..),
                                         getAmountNoUpdate, importAddress,
                                         submitTransactionRetry,
                                         updateBlockchain)
import qualified UserOptions            as O


initializeStorage
    :: forall (m :: * -> *).
       (WorkMode m)
    => U.RSCoinUserState
    -> O.UserOptions
    -> m ()
initializeStorage st O.UserOptions{..} =
    U.initState st addressesNum $ bankKeyPath isBankMode bankModePath
  where
    bankKeyPath True p  = Just p
    bankKeyPath False _ = Nothing

-- | Processes command line user command
processCommand
    :: (MonadIO m, WorkMode m)
    => U.RSCoinUserState -> O.UserCommand -> O.UserOptions -> m ()
processCommand st O.ListAddresses _ =
    eWrap $
    do res <- updateBlockchain st False
       unless res $ C.logInfo "Successfully updated blockchain."
       genAdr <- (^.C.genesisAddress) <$> getNodeContext
       addresses <- query' st $ U.GetOwnedAddresses genAdr
       (wallets :: [(C.PublicKey, C.TxStrategy, [C.Coin])]) <-
           mapM (\addr -> do
                      coins <- C.coinsToList <$> getAmountNoUpdate st addr
                      strategy <- query' st $ U.GetAddressStrategy addr
                      return ( C.getAddress addr
                             , fromMaybe C.DefaultStrategy strategy
                             , coins))
                addresses
       liftIO $ do
           TIO.putStrLn "Here's the list of your accounts:"
           TIO.putStrLn
               "# | Public ID                                    | Amount"
       mapM_ formatAddressEntry ([(1 :: Integer) ..] `zip` wallets)
  where
    spaces = "                                                   "
    formatAddressEntry :: WorkMode m
                       => (Integer, (C.PublicKey, C.TxStrategy, [C.Coin]))
                       -> m ()
    formatAddressEntry (i, (key, strategy, coins)) = do
       liftIO $ do
           TIO.putStr $ sformat (int%".  "%build%" : ") i key
           when (null coins) $ putStrLn "empty"
           unless (null coins) $ TIO.putStrLn $ show' $ head coins
           unless (length coins < 2) $
               forM_ (tail coins)
                     (TIO.putStrLn . sformat (spaces % build))
       case strategy of
           C.DefaultStrategy -> return ()
           C.MOfNStrategy m allowed -> do
               genAdr <- (^.C.genesisAddress) <$> getNodeContext
               liftIO $ do
                   TIO.putStrLn $ sformat
                        ("    This is a multisig address ("%int%"/"%int%") controlled by keys: ")
                        m (length allowed)
                   forM_ allowed $ \allowedAddr -> do
                       addresses <- query' st $ U.GetOwnedAddresses genAdr
                       TIO.putStrLn $ sformat
                           (if allowedAddr `elem` addresses
                            then "    * "%build%" owned by you"
                            else "    * "%build)
                           allowedAddr
processCommand st (O.FormTransaction inputs outputAddrStr outputCoins cache) _ =
    eWrap $
    do let outputAddr = C.Address <$> C.constructPublicKey outputAddrStr
           inputs' = map (foldr1 (\(a,b) (_,d) -> (a, b ++ d))) $
                     groupBy ((==) `on` snd) $
                     map (\(idx,o,c) -> (idx - 1, [C.Coin (C.Color c) (C.CoinAmount $ toRational o)]))
                     inputs
           outputs' = map (uncurry (flip C.Coin) . bimap (C.CoinAmount . toRational) C.Color)
                          outputCoins
           td = TransactionData
                { tdInputs = inputs'
                , tdOutputAddress = fromJust outputAddr
                , tdOutputCoins = outputs'
                }
       unless (isJust outputAddr) $
           U.commitError $ "Provided key can't be exported: " <> outputAddrStr
       tx <- submitTransactionRetry 2 st cache td
       C.logInfo $
           sformat ("Successfully submitted transaction with hash: " % build) $
               C.hash tx
processCommand st O.UpdateBlockchain _ =
    eWrap $
    do res <- updateBlockchain st True
       C.logInfo $
           if res
               then "Blockchain is updated already."
               else "Successfully updated blockchain."
processCommand st (O.Dump command) _ = eWrap $ dumpCommand st command
processCommand _ (O.SignSeed seedB64 mPath) _ = liftIO $ do
    sk <- maybe (pure C.attainSecretKey) C.readSecretKey mPath
    (seedPk, _) <- case B64.decode $ encodeUtf8 seedB64 of
              Left _ -> fail "Wrong seed supplied (base64 decoding failed)"
              Right s ->
                  maybe (fail "Failed to derive keypair from seed") pure $
                      C.deterministicKeyGen s
    liftIO $ TIO.putStrLn $
       sformat ("Seed Pk: " % build) seedPk
    let (pk, sig) = (C.derivePublicKey sk, C.sign sk seedPk)
    liftIO $ TIO.putStrLn $
       sformat ("AttPk: " % build % ", AttSig: " % build % ", verifyChain: " % build)
           pk sig (C.verifyChain pk [(sig, seedPk)])
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
    C.logInfo $
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
    C.logInfo "Address allocation successfully confirmed!"
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
        let padding = foldr1 (%) (replicate numLength " ")
        let form = int % ". " % build % "\n  " % build % padding
        liftIO $ TIO.putStrLn $ sformat form i addr allocStrat
processCommand st (O.ImportAddress skPath pkPath heightFrom heightTo) _ = do
    (sk,pk) <- liftIO $ do
        C.logInfo "Reading sk/pk from files..."
        (,) <$> C.readSecretKey skPath <*> C.readPublicKey pkPath
    C.logInfo "Starting blockchain query process"
    importAddress st (sk,pk) heightFrom heightTo
    C.logInfo "Finished, your address successfully added"

dumpCommand
    :: WorkMode m
    => U.RSCoinUserState -> O.DumpCommand -> m ()
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
    (\ctx -> query' st (U.GetOwnedAddresses (ctx^.C.genesisAddress))) =<<
    getNodeContext
