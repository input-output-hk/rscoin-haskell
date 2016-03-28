-- | Server implementation for mintette

module RSCoin.Mintette.Server
       ( serve
       ) where

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Acid.Advanced        (update')

import qualified RSCoin.Core               as C
import           RSCoin.Mintette.AcidState (FinishPeriod (..), StartPeriod (..),
                                            State)

serve :: Int -> State -> IO ()
serve port = C.serve port . handler

handler :: State -> C.MintetteReq -> IO C.MintetteRes
handler st (C.ReqPeriodFinished pId) =
    C.ResPeriodFinished <$> handlePeriodFinished st pId
handler st (C.ReqAnnounceNewPeriod d) =
    (const C.ResAnnounceNewPeriod) <$> handleNewPeriod st d
handler st (C.ReqCheckTx tx a sg) = C.ResCheckTx <$> handleCheckTx st tx a sg
handler st (C.ReqCommitTx tx pId cc) =
    C.ResCommitTx <$> handleCommitTx st tx pId cc

handlePeriodFinished :: MonadIO m => State -> C.PeriodId -> m C.PeriodResult
handlePeriodFinished st pId = update' st $ FinishPeriod pId

handleNewPeriod :: MonadIO m => State -> C.NewPeriodData -> m ()
handleNewPeriod st d = update' st $ StartPeriod d

handleCheckTx
    :: MonadIO m
    => State
    -> C.Transaction
    -> C.AddrId
    -> C.Signature
    -> m (Maybe C.CheckConfirmation)
handleCheckTx = undefined

handleCommitTx
    :: MonadIO m
    => State
    -> C.Transaction
    -> C.PeriodId
    -> C.CheckConfirmations
    -> m (Maybe C.CommitConfirmation)
handleCommitTx = undefined
