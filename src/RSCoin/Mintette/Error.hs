-- | All possible runtime errors in Mintette

module RSCoin.Mintette.Error
       ( MintetteError (..)
       , isMEInactive
       ) where

import           Control.Exception       (Exception (..), SomeException)
import           Data.MessagePack        (MessagePack (fromObject, toObject),
                                          Object)
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Data.Text.Buildable     (Buildable (build))
import qualified Data.Text.Format        as F
import           Data.Typeable           (Typeable)

import           RSCoin.Core.Error       (rscExceptionFromException,
                                          rscExceptionToException)
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
    deriving (Show, Typeable, Eq)

instance Exception MintetteError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable MintetteError where
    build (MEInternal m) = "internal error: " <> build m
    build MEInactive = "mintette is not active right now"
    build (MEPeriodMismatch expected received) =
        F.build
            "received strange PeriodId: {} (expected {})"
            (received, expected)
    build MEInvalidTxSums =
        "sum of transaction outputs is greater than sum of inputs"
    build (MEInconsistentRequest msg) = build msg
    build (MENotUnspent a) =
        F.build "can't deduce that {} is unspent transaction output" $ F.Only a
    build MEInvalidSignature = "failed to verify signature"
    build MENotConfirmed = "transaction doesn't have enough confirmations"
    build MEAlreadyActive = "can't start new period when period is active"

toObj
    :: MessagePack a
    => (Int, a) -> Object
toObj = toObject

instance MessagePack MintetteError where
    toObject (MEInternal t) = toObj (0, t)
    toObject MEInactive = toObj (1, ())
    toObject (MEPeriodMismatch p1 p2) = toObj (2, (p1, p2))
    toObject MEInvalidTxSums = toObj (3, ())
    toObject (MEInconsistentRequest t) = toObj (4, t)
    toObject (MENotUnspent a) = toObj (5, a)
    toObject MEInvalidSignature = toObj (6, ())
    toObject MENotConfirmed = toObj (7, ())
    toObject MEAlreadyActive = toObj (8, ())
    fromObject obj = do
        (i,payload) <- fromObject obj
        case (i :: Int) of
            0 -> MEInternal <$> fromObject payload
            1 -> pure MEInactive
            2 -> uncurry MEPeriodMismatch <$> fromObject payload
            3 -> pure MEInvalidTxSums
            4 -> MEInconsistentRequest <$> fromObject payload
            5 -> MENotUnspent <$> fromObject payload
            6 -> pure MEInvalidSignature
            7 -> pure MENotConfirmed
            8 -> pure MEAlreadyActive
            _ -> Nothing

isMEInactive :: SomeException -> Bool
isMEInactive = maybe False (== MEInactive) . fromException
