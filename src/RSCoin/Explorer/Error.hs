-- | All possible runtime errors in Explorer.

module RSCoin.Explorer.Error
       ( ExplorerError (..)
       ) where

import           Control.Exception   (Exception (..))
import           Data.Text.Buildable (Buildable (build))
import           Data.Typeable       (Typeable)
import           Formatting          (bprint, int, (%))

import           RSCoin.Core         (PeriodId, rscExceptionFromException,
                                      rscExceptionToException)

data ExplorerError = EEPeriodMismatch
    { pmExpectedPeriod :: PeriodId
    , pmReceivedPeriod :: PeriodId
    } deriving (Show, Typeable)

instance Exception ExplorerError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable ExplorerError where
    build EEPeriodMismatch{..} =
        bprint
            ("expected period " % int % ", but received " % int)
            pmExpectedPeriod
            pmReceivedPeriod
