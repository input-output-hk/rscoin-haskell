{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for mintette

module RSCoin.Mintette.Server
       ( serve
       ) where

import           Control.Exception         (bracket, try)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Acid.Advanced        (update')

import qualified RSCoin.Core               as C
import           RSCoin.Mintette.AcidState (CheckNotDoubleSpent (..),
                                            CommitTx (..), FinishPeriod (..),
                                            StartPeriod (..), State, closeState,
                                            openState)
import           RSCoin.Mintette.Error     (MintetteError)
import           RSCoin.Mintette.Worker    (runWorker)

serve :: Int -> FilePath -> C.SecretKey -> IO ()
serve port dbPath sk =
    bracket (openState dbPath) closeState $
    \st ->
         do runWorker sk st
            C.serve port $ handler sk st

handler :: C.SecretKey -> State -> C.MintetteReq -> IO C.MintetteRes
handler sk st (C.ReqPeriodFinished pId) =
    C.ResPeriodFinished <$> handlePeriodFinished sk st pId
handler _ st (C.ReqAnnounceNewPeriod mid d) =
    (const C.ResAnnounceNewPeriod) <$> handleNewPeriod st (mid, d)
handler sk st (C.ReqCheckTx tx a sg) = C.ResCheckTx <$> handleCheckTx sk st tx a sg
handler sk st (C.ReqCommitTx tx pId cc) =
    C.ResCommitTx <$> handleCommitTx sk st tx pId cc

handlePeriodFinished
    :: MonadIO m
    => C.SecretKey -> State -> C.PeriodId -> m C.PeriodResult
handlePeriodFinished sk st pId = update' st $ FinishPeriod sk pId

handleNewPeriod :: MonadIO m => State -> (C.MintetteId, C.NewPeriodData) -> m ()
handleNewPeriod st d = update' st $ StartPeriod d

handleCheckTx
    :: C.SecretKey
    -> State
    -> C.Transaction
    -> C.AddrId
    -> C.Signature
    -> IO (Maybe C.CheckConfirmation)
handleCheckTx sk st tx addrId sg = do
    (res :: Either MintetteError C.CheckConfirmation) <-
        try $ update' st $ CheckNotDoubleSpent sk tx addrId sg
    either (const $ return Nothing) (return . Just) res

handleCommitTx
    :: C.SecretKey
    -> State
    -> C.Transaction
    -> C.PeriodId
    -> C.CheckConfirmations
    -> IO (Maybe C.CommitConfirmation)
handleCommitTx sk st tx pId cc = do
    (res :: Either MintetteError C.CommitConfirmation) <-
        try $ update' st $ CommitTx sk tx pId cc
    either (const $ return Nothing) (return . Just) res
