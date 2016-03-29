{-# LANGUAGE ScopedTypeVariables #-}
-- | This module reperents all logic that abstract client should have
-- in its arsenal -- mostly the algorithmic part of user in paper,
-- requests to bank/mintettes and related things.

module RSCoin.User.Logic
       ( CC.getBlockByHeight
       , CC.getBlockchainHeight
       , validateTransaction
       ) where

import           RSCoin.Core.CheckConfirmation (verifyCheckConfirmation)
import qualified RSCoin.Core.Communication     as CC
import           RSCoin.Core.Crypto            (Signature, verify)
import           RSCoin.Core.Primitives        (AddrId, Transaction (..))
import           RSCoin.Core.Types             (CheckConfirmation (..),
                                                CheckConfirmations,
                                                CommitConfirmation, Mintette,
                                                MintetteId, PeriodId)

import           Serokell.Util.Text            (format')

import           Control.Exception             (Exception, throwIO)
import           Control.Monad                 (unless, when)
import qualified Data.Map                      as M
import           Data.Maybe                    (catMaybes)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T

data UserLogicError
    = MintetteSignatureFailed Mintette
    | MajorityRejected T.Text
    | FailedToCommit
    deriving (Show)

instance Exception UserLogicError

-- | Implements V.1 from the paper
validateTransaction :: Transaction -> Signature -> PeriodId -> IO ()
validateTransaction tx@Transaction{..} sig height = do
    (bundle :: CheckConfirmations) <- mconcat <$> mapM processInput txInputs
    commitBundle bundle
  where
    processInput :: AddrId -> IO CheckConfirmations
    processInput addrid = do
        owns <- CC.getOwnersByAddrid addrid
        unless (length owns >= 2) $
            throwIO $
            MajorityRejected $
            format'
                "Got only {} owners of addrid {} -- that's not enough to pass majority test."
                (length owns, addrid)
        -- TODO maybe optimize it: we shouldn't query all mintettes, only the majority
        subBundle <- mconcat . catMaybes <$> mapM (processMintette addrid) owns
        when (length subBundle < length owns `div` 2) $
            throwIO $
            MajorityRejected $
            format'
                ("Couldn't get CheckNotDoubleSpent " <>
                 "from majority of mintettes: only {}/{} confirmed {} is not double-spent.")
                (length subBundle, length owns, addrid)
        return subBundle
    processMintette :: AddrId
                    -> (Mintette, MintetteId)
                    -> IO (Maybe CheckConfirmations)
    processMintette addrid (mintette,mid) = do
        signedPairMb <- CC.checkNotDoubleSpent mintette tx addrid sig
        maybe
            (return Nothing)
            (\proof -> do unless (verifyCheckConfirmation proof tx addrid) $
                              throwIO $ MintetteSignatureFailed mintette
                          return $ Just $ M.singleton (mid, addrid) proof)
            signedPairMb
    commitBundle :: CheckConfirmations -> IO ()
    commitBundle bundle = do
        owns <- CC.getOwnersByTx tx
        (succeededCommits :: [CommitConfirmation]) <-
            filter (\(pk, sign, lch) -> verify pk sign (tx, lch)) . catMaybes <$>
            mapM ((\mintette -> CC.commitTx mintette tx height bundle) . fst) owns
        unless (null succeededCommits) $ throwIO FailedToCommit
