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

handlePeriodFinished :: MonadIO m => State -> C.PeriodId -> m C.PeriodResult
handlePeriodFinished st pId = update' st $ FinishPeriod pId

handleNewPeriod :: MonadIO m => State -> C.NewPeriodData -> m ()
handleNewPeriod st d = update' st $ StartPeriod d
