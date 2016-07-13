-- | All possible runtime errors in Explorer.

module RSCoin.Explorer.Error
       ( ExplorerError (..)
       ) where

import           Control.Exception   (Exception (..))
import           Data.Text           (Text)
import           Data.Text.Buildable (Buildable (build))
import           Data.Typeable       (Typeable)
import           Formatting          (bprint, int, stext, (%))

import           RSCoin.Core         (PeriodId, rscExceptionFromException,
                                      rscExceptionToException)

data ExplorerError
    = EEPeriodMismatch { pmExpectedPeriod :: PeriodId
                       , pmReceivedPeriod :: PeriodId}
    | EEInternalError Text
    | EEInvalidBankSignature
    deriving (Show,Typeable)

instance Exception ExplorerError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable ExplorerError where
    build EEPeriodMismatch{..} =
        bprint
            ("expected period " % int % ", but received " % int)
            pmExpectedPeriod
            pmReceivedPeriod
    build (EEInternalError msg) = bprint ("internal error: " % stext) msg
    build EEInvalidBankSignature = "signature given by bank is not valid"
