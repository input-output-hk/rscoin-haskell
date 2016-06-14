{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module reperents all logic that abstract client should have
-- in its arsenal -- mostly the algorithmic part of user in paper,
-- requests to bank/mintettes and related things.

module RSCoin.User.Logic
       ( CC.getBlockByHeight
       , CC.getBlockchainHeight
       , validateTransaction
       ) where

import           Control.Monad                 (guard, unless, when)
import           Control.Monad.Catch           (throwM)
import           Control.Monad.Trans           (liftIO)
import           Data.Either.Combinators       (fromLeft', isLeft, rightToMaybe)
import           Data.List                     (genericLength)
import qualified Data.Map                      as M
import           Data.Maybe                    (catMaybes, fromJust)
import           Data.Monoid                   ((<>))

import           RSCoin.Core.CheckConfirmation (verifyCheckConfirmation)
import qualified RSCoin.Core.Communication     as CC
import           RSCoin.Core.Crypto            (Signature, verify)
import           RSCoin.Core.Logging           (logWarning, userLoggerName)
import           RSCoin.Core.Primitives        (AddrId, Transaction (..))
import           RSCoin.Core.Types             (CheckConfirmations,
                                                CommitConfirmation, Mintette,
                                                MintetteId, PeriodId)
import           RSCoin.Mintette.Error         (MintetteError)
import           RSCoin.Timed                  (WorkMode)
import           RSCoin.User.Cache             (UserCache, getOwnersByAddrid,
                                                getOwnersByTx,
                                                invalidateUserCache)
import           RSCoin.User.Error             (UserLogicError (..))

import           Serokell.Util.Text            (format', formatSingle',
                                                listBuilderJSON, pairBuilder)

-- | Implements V.1 from the paper. For all addrids that are inputs of
-- transaction 'signatures' should contain signature of transaction
-- given. If transaction is confirmed, just returns. If it's not
-- confirmed, the MajorityFailedToCommit is thrown.
validateTransaction
    :: WorkMode m
    => Maybe UserCache
    -> Transaction
    -> M.Map AddrId Signature
    -> PeriodId
    -> m ()
validateTransaction cache tx@Transaction{..} signatures height = do
    (bundle :: CheckConfirmations) <- mconcat <$> mapM processInput txInputs
    commitBundle bundle
  where
    processInput
        :: WorkMode m
        => AddrId -> m CheckConfirmations
    processInput addrid = do
        owns <- getOwnersByAddrid cache height addrid
        when (null owns) $
            throwM $
            MajorityRejected $
            formatSingle' "Addrid {} doesn't have owners" addrid
        -- TODO maybe optimize it: we shouldn't query all mintettes, only the majority
        subBundle <- mconcat . catMaybes <$> mapM (processMintette addrid) owns
        when (length subBundle <= length owns `div` 2) $
            do invalidateCache
               throwM $
                   MajorityRejected $
                   format'
                       ("Couldn't get CheckNotDoubleSpent " <>
                        "from majority of mintettes: only {}/{} confirmed {} is not double-spent.")
                       (length subBundle, length owns, addrid)
        return subBundle
    processMintette
        :: WorkMode m
        => AddrId -> (Mintette, MintetteId) -> m (Maybe CheckConfirmations)
    processMintette addrid (mintette,mid) = do
        signedPairMb <-
            rightToMaybe <$>
            (CC.checkNotDoubleSpent mintette tx addrid $
             fromJust $ M.lookup addrid signatures)
        return $
            signedPairMb >>=
            \proof ->
                 M.singleton (mid, addrid) proof <$
                 guard (verifyCheckConfirmation proof tx addrid)
    commitBundle
        :: WorkMode m
        => CheckConfirmations -> m ()
    commitBundle bundle = do
        owns <- getOwnersByTx cache height tx
        commitActions <-
            mapM
                (\(mintette,_) ->
                      CC.commitTx mintette tx height bundle)
                owns
        let succeededCommits :: [CommitConfirmation]
            succeededCommits =
                filter
                    (\(pk,sign,lch) ->
                          verify pk sign (tx, lch)) $
                catMaybes $ map rightToMaybe $ commitActions
            failures = filter isLeft commitActions
        unless (null failures) $
            logWarning userLoggerName $
            commitTxWarningMessage owns commitActions
        when (length succeededCommits <= length owns `div` 2) $
            do throwM $
                   MajorityFailedToCommit
                       (genericLength succeededCommits)
                       (genericLength owns)
    commitTxWarningMessage owns =
        formatSingle'
            "some mintettes returned error in response to `commitTx`: {}" .
        listBuilderJSON . map pairBuilder . mintettesAndErrors owns
    mintettesAndErrors
        :: [(Mintette, MintetteId)]
        -> [Either MintetteError CommitConfirmation]
        -> [(Mintette, MintetteError)]
    mintettesAndErrors owns =
        map sndFromLeft . filter (isLeft . snd) . zip (map fst owns)
    sndFromLeft :: (a, Either MintetteError b) -> (a, MintetteError)
    sndFromLeft (a,b) = (a, fromLeft' b)
    invalidateCache
        :: WorkMode m
        => m ()
    invalidateCache = liftIO $ maybe (return ()) invalidateUserCache cache
