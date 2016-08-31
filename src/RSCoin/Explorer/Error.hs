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
    = EEPeriodMismatch { pmExpectedPeriod :: !PeriodId
                       , pmReceivedPeriod :: !PeriodId}
    | EENotFound !Text
    | EEInternalError !Text
    | EEInvalidBankSignature
    | EEIncorrectBlock !PeriodId !Text
    deriving (Show,Typeable)

instance Exception ExplorerError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable ExplorerError where
    build EEPeriodMismatch {..} =
        bprint
            ("expected period " % int % ", but received " % int)
            pmExpectedPeriod
            pmReceivedPeriod
    build (EENotFound msg) = bprint ("resource not found: " % stext) msg
    build (EEInternalError msg) = bprint ("internal error: " % stext) msg
    build EEInvalidBankSignature = "signature given by bank is not valid"
    build (EEIncorrectBlock pId reason) =
        bprint
            ("received block #" % int % " is not valid, reason: " % stext)
            pId
            reason
