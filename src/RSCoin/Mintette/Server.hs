-- | Server implementation for mintette

module RSCoin.Mintette.Server
       ( serve
       ) where

import           Control.Exception         (bracket)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans       (lift)
import           Data.Acid.Advanced        (update')

import qualified RSCoin.Core               as C
import           RSCoin.Mintette.AcidState (CheckNotDoubleSpent (..),
                                            CommitTx (..), FinishPeriod (..),
                                            StartPeriod (..), State, closeState,
                                            openState)
import           RSCoin.Mintette.Worker    (runWorker)

serve :: Int -> FilePath -> C.SecretKey -> IO ()
serve port dbPath sk =
    bracket (openState dbPath) closeState $
    \st ->
         do runWorker sk st
            C.serve port
                [ C.method (C.RSCMintette C.PeriodFinished) $ fmap C.AsMessagePack . handlePeriodFinished sk st
                , C.method (C.RSCMintette C.AnnounceNewPeriod) $ handleNewPeriod st
                , C.method (C.RSCMintette C.CheckTx) $ handleCheckTx sk st
                , C.method (C.RSCMintette C.CommitTx) $ handleCommitTx sk st
                ]

handlePeriodFinished
    :: MonadIO m
    => C.SecretKey -> State -> C.PeriodId -> m C.PeriodResult
handlePeriodFinished sk st pId = update' st $ FinishPeriod sk pId

handleNewPeriod :: MonadIO m => State -> C.NewPeriodData -> m ()
handleNewPeriod st d = update' st $ StartPeriod d

handleCheckTx
    :: MonadIO m
    => C.SecretKey
    -> State
    -> C.Transaction
    -> C.AddrId
    -> C.Signature
    -> m (Maybe C.CheckConfirmation)
handleCheckTx sk st tx addrId sg =
    update' st $ CheckNotDoubleSpent sk tx addrId sg

handleCommitTx
    :: MonadIO m
    => C.SecretKey
    -> State
    -> C.Transaction
    -> C.PeriodId
    -> C.CheckConfirmations
    -> m (Maybe C.CommitConfirmation)
handleCommitTx sk st tx pId cc = update' st $ CommitTx sk tx pId cc
