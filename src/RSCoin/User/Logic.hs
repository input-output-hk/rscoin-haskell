{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | This module reperents all logic that abstract client should have
-- in its arsenal -- mostly the algorithmic part of user in paper,
-- requests to bank/mintettes and related things.

module RSCoin.User.Logic
       ( CC.getBlockByHeight
       , CC.getBlockchainHeight
       , SignatureBundle
       , SignatureValue
       , joinBundles
       , getExtraSignatures
       , validateTransaction
       ) where

import           Control.Arrow                 (second)
import           Control.Lens                  (view, _1, _2, _3)
import           Control.Monad                 (guard, unless, when)
import           Control.Monad.Catch           (throwM)
import           Control.Monad.Trans           (liftIO)
import           Data.Either                   (partitionEithers)
import           Data.Either.Combinators       (fromLeft', isLeft, rightToMaybe)
import           Data.List                     (genericLength, nub)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromJust, mapMaybe, maybeToList)
import qualified Data.Text                     as T
import           Data.Time.Units               (Second)
import           Formatting                    (build, int, sformat, (%))

import           Serokell.Util.Text            (listBuilderJSON, pairBuilder)

import qualified RSCoin.Core                   as C
import           RSCoin.Core.CheckConfirmation (verifyCheckConfirmation)
import qualified RSCoin.Core.Communication     as CC
import           RSCoin.Core.Crypto            (Signature, verify)
import           RSCoin.Core.Logging           (logInfo, logWarning)
import           RSCoin.Core.Primitives        (AddrId, Address,
                                                Transaction (..))
import           RSCoin.Core.Strategy          (TxStrategy (..),
                                                isStrategyCompleted)
import           RSCoin.Core.Types             (CheckConfirmation,
                                                CheckConfirmations,
                                                CommitAcknowledgment (..),
                                                Mintette, MintetteId, PeriodId)

import           Control.TimeWarp.Timed        (for, ms, timeout, wait)
import           RSCoin.User.Cache             (UserCache, getOwnersByAddrid,
                                                getOwnersByTx,
                                                invalidateUserCache)
import           RSCoin.User.Error             (UserLogicError (..))


-- | SignatureBundle is a datatype that represents signatures needed
-- to prove that address owners are OK with transaction spending money
-- from that address
-- @TODO: these types are awful :(
type SignatureValue  = (Address, TxStrategy, [(Address, Signature Transaction)])
type SignatureBundle = M.Map AddrId SignatureValue

-- | This type represents for each unique address in the given transaction:
-- * Strategy of working on that address
-- * Addrids that spend money from that address in tx
-- * User's permission to spend money from address
type AddressSignInfo = (TxStrategy, [AddrId], (Address,Signature Transaction))

joinBundles
    :: (a, b, [(Address, Signature Transaction)])
    -> (a, b, [(Address, Signature Transaction)])
    -> (a, b, [(Address, Signature Transaction)])
joinBundles (a,s,signs1) (_,_,signs2) = (a,s,nub $ signs1 ++ signs2)

-- | Gets signatures that can't be retrieved locally (for strategies
-- other than local).
getExtraSignatures
    :: C.WorkMode m
    => Transaction                   -- ^ Transaction to confirm addrid from
    -> M.Map Address AddressSignInfo -- ^ For each address in tx input we provide
                                     -- info about strategy, addrids that reperesent
                                     -- spending, and (address,signature) pair
                                     -- prooving that this spending is signed
    -> Second                        -- ^ Timeout in seconds
    -> m (Maybe SignatureBundle)     -- ^ Nothing means transaction was
                                     -- already sent by someone, (Just sgs)
                                     -- means user should send it.
getExtraSignatures tx requests time = do
    unless checkInput $ error "Wrong input of getExtraSignatures"
    (waiting,ready0) <- partitionEithers <$> mapM perform (M.keys requests)
    let ready = map snd ready0
        itsCoolAlready = all fst ready0
    if null waiting
    then return (if itsCoolAlready then Just (toBundle ready) else Nothing)
    else do
        timeoutRes <- timeout time $ do
            logInfo "Waiting for others to sign."
            pingUntilDead waiting []
        return $ Just $ toBundle $ ready ++ timeoutRes
  where
    lookupMap addr = fromJust $ M.lookup addr requests
    getStrategy = view _1 . lookupMap
    getAddrIds = view _2 . lookupMap
    getOwnSignaturePair = view _3 . lookupMap
    checkInput = all (`elem` txInputs tx) $ concatMap (view _2)$ M.elems requests
    toBundle =
        M.fromListWith joinBundles .
        concatMap (\(addr,signs) ->
            let str = getStrategy addr
                addrids = getAddrIds addr
            in map (,(addr,str,signs)) addrids)
    pingUntilDead [] ready = return ready
    pingUntilDead notReady@(addr:otherAddrs) ready = do
        wait $ for 400 ms
        sigs <- CC.getTxSignatures tx addr
        if isStrategyCompleted (getStrategy addr) addr sigs tx
        then pingUntilDead otherAddrs $ (addr,sigs):ready
        else pingUntilDead notReady ready
    -- Returns (Right signs) if for address:
    -- 1. First poll showed signatures are ready
    -- 2. After our signature commit signatures became ready
    -- Returns (Left addr) if Notary should be polled for transaction tx
    -- and addr `addr` and sigs are not ready
    perform :: C.WorkMode m
            => Address
            -> m (Either Address (Bool, (Address, [(Address, Signature Transaction)])))
    perform addr = do
        let returnRight b s = return $ Right (b,(addr,s))
            strategy = getStrategy addr
            ownSgPair = getOwnSignaturePair addr
        curSigs <- CC.getTxSignatures tx addr
        if isStrategyCompleted strategy addr curSigs tx
        then returnRight False curSigs
        else do
            afterPublishSigs <- CC.publishTxToNotary tx addr ownSgPair
            if isStrategyCompleted strategy addr afterPublishSigs tx
            then returnRight True afterPublishSigs
            else return $ Left addr

-- | Just a convenient alias
type Owner = (Mintette,MintetteId)

-- | Implements V.1 from the paper. For all addrids that are inputs of
-- transaction 'signatures' should contain signature of transaction
-- given. If transaction is confirmed, just returns. If it's not
-- confirmed, the MajorityFailedToCommit is thrown.
validateTransaction
    :: C.WorkMode m
    => Maybe UserCache -- ^ User cache
    -> Transaction     -- ^ Transaction to send
    -> SignatureBundle -- ^ Signatures for local addresses with default strategy
    -> PeriodId        -- ^ Period in which the transaction should be sent
    -> m ()
validateTransaction cache tx@Transaction{..} signatureBundle periodId = do
    unless (all checkStrategy $ M.elems signatureBundle) $ throwM StrategyFailed
    (bundle :: CheckConfirmations) <- getConfirmations
--    (bundle :: CheckConfirmations) <- mconcat <$> mapM processInput txInputs
    commitBundle bundle
  where
    checkStrategy :: (Address, TxStrategy, [(Address, Signature Transaction)]) -> Bool
    checkStrategy (addr,str,sgns) = isStrategyCompleted str addr sgns tx
    reverseMap :: (Ord b) => M.Map a [b] -> M.Map b [a]
    reverseMap orig =
        let allElems = nub $ concat $ M.elems orig
            assocs = M.assocs orig
        in M.fromList [ (b, map fst $ filter ((b `elem`) . snd) assocs)
                      | b <- allElems ]
    retrieveOwners :: (C.WorkMode m) => AddrId -> m [(Mintette,MintetteId)]
    retrieveOwners addrid = do
        owns <- getOwnersByAddrid cache periodId addrid
        when (null owns) $
            throwM $
            MajorityRejected $
            sformat ("Addrid " % build % " doesn't have owners") addrid
        return owns
    getConfirmations :: (C.WorkMode m) => m CheckConfirmations
    getConfirmations = do
        -- Get the mapping from addrids to owners it has
        (requests :: M.Map AddrId [Owner]) <- M.fromList <$>
            mapM (\addrid -> (addrid,) <$> retrieveOwners addrid) txInputs
        -- Getting the shard size and reversed map
        let shardSize :: Int
            shardSize = length $ snd $ head $ M.assocs requests
            revRequests :: M.Map Owner [AddrId]
            revRequests = reverseMap requests
        -- Querying each mintette and getting a map for their answers
        (confirmations :: M.Map Owner [(AddrId, Maybe CheckConfirmation)]) <-
            M.fromList <$>
            mapM (\(owner,addrids) -> do
                -- Those are only the part of signature bundle related to the
                -- addrids the mintette must process
                let bundlePart =
                        M.filterWithKey (\addrid _ -> addrid `elem` addrids) $
                            M.map (view _3) signatureBundle
                response <-
                    M.map rightToMaybe <$>
                    CC.checkNotDoubleSpentBatch (fst owner) tx bundlePart
                let verifyConfirmation addrid proofMaybe = do
                        proof <- proofMaybe
                        guard $ verifyCheckConfirmation proof tx addrid periodId
                        return proof
                return (owner, M.assocs $ M.mapWithKey verifyConfirmation response))
            (M.assocs revRequests)
        -- Reversing this map to something from addrid to (owner,[confirmations]
        let revConfirmations :: M.Map AddrId [(MintetteId,CheckConfirmation)]
            revConfirmations =
                M.fromListWith (++) $
                map (\((addrid,cc),(_,mId)) ->
                      (addrid,maybeToList ((mId,) <$> cc))) $
                map (second head) $
                M.assocs $ reverseMap confirmations
            failedConfirmsPred (_,allconfs) =
                length allconfs <= shardSize `div` 2
            failedConfirms = filter failedConfirmsPred $ M.assocs revConfirmations
        unless (null failedConfirms) $ do
            let (addrid,allConfs) = head failedConfirms
            invalidateCache
            throwM $
                 MajorityRejected $
                 sformat
                     ("Couldn't get CheckNotDoubleSpent " %
                      "from majority of mintettes: only " %
                      int % "/" % int % " confirmed " % build %
                      " is not double-spent.")
                     (length allConfs) shardSize addrid
        let checkConfirmations :: CheckConfirmations
            checkConfirmations =
                M.fromList $
                concatMap (\(addrid,confs) ->
                            map (\(mId,conf) -> ((mId,addrid),conf)) confs) $
                M.assocs revConfirmations
        return checkConfirmations
    commitBundle
        :: C.WorkMode m
        => CheckConfirmations -> m ()
    commitBundle bundle = do
        owns <- getOwnersByTx cache periodId tx
        commitActions <-
            mapM
                (\(mintette,_) ->
                      CC.commitTx mintette tx bundle)
                owns
        let succeededCommits :: [CommitAcknowledgment]
            succeededCommits = filter
                (\(CommitAcknowledgment pk sign lch) -> verify pk sign (tx, lch))
                $ mapMaybe rightToMaybe commitActions
        let failures = filter isLeft commitActions
        unless (null failures) $
            logWarning $
            commitTxWarningMessage owns commitActions
        when (length succeededCommits <= length owns `div` 2) $
            throwM $ MajorityFailedToCommit
                 (genericLength succeededCommits)
                 (genericLength owns)
    commitTxWarningMessage owns =
        sformat
            ("Some mintettes returned error in response to `commitTx`: " % build) .
        listBuilderJSON . map pairBuilder . mintettesAndErrors owns
    mintettesAndErrors
        :: [(Mintette, MintetteId)]
        -> [Either T.Text CommitAcknowledgment]
        -> [(Mintette, T.Text)]
    mintettesAndErrors owns =
        map sndFromLeft . filter (isLeft . snd) . zip (map fst owns)
    sndFromLeft :: (a, Either T.Text b) -> (a, T.Text)
    sndFromLeft (a,b) = (a, fromLeft' b)
    invalidateCache
        :: C.WorkMode m
        => m ()
    invalidateCache = liftIO $ maybe (return ()) invalidateUserCache cache
