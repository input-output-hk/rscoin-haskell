-- | All possible runtime errors in Mintette

module RSCoin.Mintette.Error
       ( MintetteError (..)
       , isMEInactive
       , logMintetteError
       ) where

import           Control.Exception       (Exception (..), SomeException)
import           Control.Monad.Trans     (MonadIO)
import           Data.MessagePack        (MessagePack (fromObject, toObject), Object)
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text.Buildable     as B (Buildable (build))
import           Data.Typeable           (Typeable)
import           Formatting              (bprint, build, int, sformat, stext, (%))

import           RSCoin.Core.Error       (rscExceptionFromException,
                                          rscExceptionToException)
import           RSCoin.Core.Logging     (WithNamedLogger, logInfo, logWarning)
import           RSCoin.Core.MessagePack ()
import           RSCoin.Core.Primitives  (AddrId)
import           RSCoin.Core.Types       (PeriodId)

data MintetteError
    = MEInternal Text                     -- ^ Should not happen.
    | MEInactive                          -- ^ Mintette is not active right now.
    | MEPeriodMismatch PeriodId PeriodId  -- ^ PeriodId expected by mintette is
                                          -- different from the one expected by somebody.
    | MEInvalidTxSums                     -- ^ Mintette received transaction with invalid sums.
    | MEInconsistentRequest Text          -- ^ Inconsistency detected.
    | MENotUnspent AddrId                 -- ^ Given addrId is not an unspent output.
    | MEInvalidSignature                  -- ^ Signature check failed.
    | MENotConfirmed                      -- ^ Can't deduce that transaction was confirmed.
    | MEAlreadyActive                     -- ^ Can't start new period because mintette
                                          -- is already active.
    | MEInvalidBankSignature              -- ^ Bank's signature can't be verified.
    | MENotAllowed                        -- ^ Tried to send transaction that spends money
                                          -- from blacklisted address
    deriving (Show, Typeable, Eq)

instance Exception MintetteError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance B.Buildable MintetteError where
    build (MEInternal m) = "internal error: " <> B.build m
    build MEInactive = "mintette is not active right now"
    build (MEPeriodMismatch expected received) =
        bprint
            ("received strange PeriodId: " % int % " (expected " % int % ")")
            received expected
    build MEInvalidTxSums =
        "sum of transaction outputs is greater than sum of inputs"
    build (MEInconsistentRequest msg) = B.build msg
    build (MENotUnspent a) =
        bprint ("can't deduce that " % build % " is unspent transaction output") a
    build MEInvalidSignature = "failed to verify signature"
    build MENotConfirmed = "transaction doesn't have enough confirmations"
    build MEAlreadyActive = "can't start new period when period is active"
    build MEInvalidBankSignature = "bank's signature can't be verified"
    build MENotAllowed =
        "tried to send transaction that spends money from blacklisted address"

toObj :: MessagePack a => (Int, a) -> Object
toObj = toObject

instance MessagePack MintetteError where
    toObject (MEInternal t)            = toObj (0, t)
    toObject MEInactive                = toObj (1, ())
    toObject (MEPeriodMismatch p1 p2)  = toObj (2, (p1, p2))
    toObject MEInvalidTxSums           = toObj (3, ())
    toObject (MEInconsistentRequest t) = toObj (4, t)
    toObject (MENotUnspent a)          = toObj (5, a)
    toObject MEInvalidSignature        = toObj (6, ())
    toObject MENotConfirmed            = toObj (7, ())
    toObject MEAlreadyActive           = toObj (8, ())
    toObject MEInvalidBankSignature    = toObj (9, ())
    toObject MENotAllowed              = toObj (10, ())
    fromObject obj = do
        (i,payload) <- fromObject obj
        case (i :: Int) of
            0  -> MEInternal <$> fromObject payload
            1  -> pure MEInactive
            2  -> uncurry MEPeriodMismatch <$> fromObject payload
            3  -> pure MEInvalidTxSums
            4  -> MEInconsistentRequest <$> fromObject payload
            5  -> MENotUnspent <$> fromObject payload
            6  -> pure MEInvalidSignature
            7  -> pure MENotConfirmed
            8  -> pure MEAlreadyActive
            9  -> pure MEInvalidBankSignature
            10 -> pure MENotAllowed
            _  -> Nothing

isMEInactive :: SomeException -> Bool
isMEInactive = maybe False (== MEInactive) . fromException

logMintetteError
    :: (MonadIO m, WithNamedLogger m)
    => MintetteError -> Text -> m ()
logMintetteError e msg =
    case e of
        MEInactive -> logInfo toPrint
        _          -> logWarning toPrint
  where
    toPrint = sformat (stext % ", error: " % Formatting.build) msg e
