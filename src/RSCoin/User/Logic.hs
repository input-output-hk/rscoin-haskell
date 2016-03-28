{-# LANGUAGE ScopedTypeVariables #-}
-- | This module reperents all logic that abstract client should have
-- in its arsenal -- mostly the algorithmic part of user in paper,
-- requests to bank/mintettes and related things.

module RSCoin.User.Logic
       ( CC.getBlockByHeight
       , CC.getBlockchainHeight
       , validateTransaction
       ) where

import qualified RSCoin.Core.Communication as CC
import           RSCoin.Core.Crypto        (PublicKey, Signature, verify)
import           RSCoin.Core.Primitives    (AddrId, Transaction (..))
import           RSCoin.Core.Types         (Mintette, MintetteId, PeriodId)

import           Serokell.Util.Text        (format')

import           Control.Exception         (Exception, throwIO)
import           Control.Monad             (unless, when)
import           Data.Maybe                (catMaybes)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T

data UserLogicError
    = MintetteSignatureFailed Mintette
    | MajorityRejected T.Text
    | FailedToCommit
    deriving (Show)

instance Exception UserLogicError

-- | Implements V.1 from the paper (without actionlog though TODO)
validateTransaction :: Transaction -> PeriodId -> IO ()
validateTransaction tx@Transaction{..} height = do
    (bundle :: CC.BundleOfEvidence) <- concat <$> mapM processInput txInputs
    commitBundle bundle
  where
    processInput :: AddrId -> IO CC.BundleOfEvidence
    processInput addrid = do
        owns <- CC.getOwnersByAddrid addrid
        unless (length owns >= 2) $
            throwIO $
            MajorityRejected $
            format'
                "Got only {} owners of addrid {} -- that's not enough to pass majority test."
                (length owns, addrid)
        -- TODO maybe optimize it: we shouldn't query all mintettes, only the majority
        subBundle <- catMaybes <$> mapM (processMintette addrid) owns
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
                    -> IO (Maybe CC.NotDoubleSpentProof)
    processMintette addrid (mintette,mid) = do
        signedPairMb <- CC.checkNotDoubleSpent tx addrid mintette mid
        maybe
            (return Nothing)
            (\proof@(pk,sg,d) ->
                  do unless (verify pk sg d) $
                         throwIO $ MintetteSignatureFailed mintette
                     return $ Just ((mid, addrid), proof))
            signedPairMb
    commitBundle :: CC.BundleOfEvidence -> IO ()
    commitBundle bundle = do
        owns <- CC.getOwnersByTx tx
        (succeededCommits :: [(PublicKey, Signature, Transaction)]) <-
            filter (uncurry3 verify) . catMaybes <$>
            mapM (uncurry $ CC.commitTransaction tx height bundle) owns
        unless (null succeededCommits) $ throwIO FailedToCommit
    uncurry3 f (a,b,c) = f a b c
