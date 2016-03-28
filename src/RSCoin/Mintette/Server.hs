-- | Server implementation for mintette

module RSCoin.Mintette.Server
       ( serve
       ) where

import           Control.Exception         (bracket)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Acid.Advanced        (update')

import qualified RSCoin.Core               as C
import           RSCoin.Mintette.AcidState (CheckNotDoubleSpent (..),
                                            FinishPeriod (..), StartPeriod (..),
                                            State, closeState, openState)

serve :: Int -> FilePath -> C.SecretKey -> IO ()
serve port dbPath sk =
    bracket (openState dbPath) closeState $ C.serve port . handler sk

handler :: C.SecretKey -> State -> C.MintetteReq -> IO C.MintetteRes
handler _ st (C.ReqPeriodFinished pId) =
    C.ResPeriodFinished <$> handlePeriodFinished st pId
handler _ st (C.ReqAnnounceNewPeriod d) =
    (const C.ResAnnounceNewPeriod) <$> handleNewPeriod st d
handler sk st (C.ReqCheckTx tx a sg) = C.ResCheckTx <$> handleCheckTx sk st tx a sg
handler _ st (C.ReqCommitTx tx pId cc) =
    C.ResCommitTx <$> handleCommitTx st tx pId cc

handlePeriodFinished :: MonadIO m => State -> C.PeriodId -> m C.PeriodResult
handlePeriodFinished st pId = update' st $ FinishPeriod pId

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
    => State
    -> C.Transaction
    -> C.PeriodId
    -> C.CheckConfirmations
    -> m (Maybe C.CommitConfirmation)
handleCommitTx = undefined
