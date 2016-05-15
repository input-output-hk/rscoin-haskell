{-# LANGUAGE ScopedTypeVariables #-}
-- | This module reperents all logic that abstract client should have
-- in its arsenal -- mostly the algorithmic part of user in paper,
-- requests to bank/mintettes and related things.

module RSCoin.User.Logic
       ( CC.getBlockByHeight
       , CC.getBlockchainHeight
       , validateTransaction
       ) where

import           RSCoin.Core                   (logError,
                                                rscExceptionFromException,
                                                rscExceptionToException,
                                                userLoggerName)
import           RSCoin.Core.CheckConfirmation (verifyCheckConfirmation)
import qualified RSCoin.Core.Communication     as CC
import           RSCoin.Core.Crypto            (Signature, verify)
import           RSCoin.Core.Primitives        (AddrId, Transaction (..))
import           RSCoin.Core.Types             (CheckConfirmations,
                                                CommitConfirmation, Mintette,
                                                MintetteId, PeriodId)
import           RSCoin.Timed                  (WorkMode)

import           Serokell.Util.Text            (format', formatSingle')

import           Control.Exception             (Exception (..))
import           Control.Monad                 (unless, when)
import           Control.Monad.Catch           (throwM)
import qualified Data.Map                      as M
import           Data.Maybe                    (catMaybes, fromJust)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T

data UserLogicError
    = MintetteSignatureFailed Mintette
    | MajorityRejected T.Text
    | FailedToCommit
    deriving (Show)

throwUserLogicError :: WorkMode m => UserLogicError -> m a
throwUserLogicError e = do
    logError userLoggerName $ T.pack $ show e
    throwM e

instance Exception UserLogicError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

-- | Implements V.1 from the paper. For all addrids that are inputs of
-- transaction 'signatures' should contain signature of transaction
-- given. If transaction is confirmed, just returns. If it's not
-- confirmed, the FailedToCommit is thrown.
validateTransaction :: WorkMode m =>
                       Transaction -> M.Map AddrId Signature -> PeriodId -> m ()
validateTransaction tx@Transaction{..} signatures height = do
    (bundle :: CheckConfirmations) <- mconcat <$> mapM processInput txInputs
    commitBundle bundle
  where
    processInput :: WorkMode m => AddrId -> m CheckConfirmations
    processInput addrid = do
        owns <- CC.getOwnersByAddrid addrid
        unless (not (null owns)) $
            throwUserLogicError $
            MajorityRejected $
            formatSingle' "Addrid {} doesn't have owners" addrid
        -- TODO maybe optimize it: we shouldn't query all mintettes, only the majority
        subBundle <- mconcat . catMaybes <$> mapM (processMintette addrid) owns
        when (length subBundle < length owns `div` 2) $
            throwUserLogicError $
            MajorityRejected $
            format'
                ("Couldn't get CheckNotDoubleSpent " <>
                 "from majority of mintettes: only {}/{} confirmed {} is not double-spent.")
                (length subBundle, length owns, addrid)
        return subBundle
    processMintette :: WorkMode m
                    => AddrId
                    -> (Mintette, MintetteId)
                    -> m (Maybe CheckConfirmations)
    processMintette addrid (mintette,mid) = do
        signedPairMb <-
            CC.checkNotDoubleSpent mintette tx addrid $
            fromJust $ M.lookup addrid signatures
        return $ signedPairMb >>= \proof ->
                    do unless (verifyCheckConfirmation proof tx addrid) $
                            throwM $ MintetteSignatureFailed mintette
                       return $ M.singleton (mid, addrid) proof
    commitBundle :: WorkMode m => CheckConfirmations -> m ()
    commitBundle bundle = do
        owns <- CC.getOwnersByTx tx
        (succeededCommits :: [CommitConfirmation]) <-
            filter
                (\(pk,sign,lch) ->
                      verify pk sign (tx, lch)) .
            catMaybes <$>
            mapM
                ((\mintette ->
                       CC.commitTx mintette tx height bundle) .
                 fst)
                owns
        when (null succeededCommits) $ throwUserLogicError FailedToCommit
