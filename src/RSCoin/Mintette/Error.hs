{-# LANGUAGE TemplateHaskell #-}

-- | All possible runtime errors in Mintette

module RSCoin.Mintette.Error
       ( MintetteError (..)
       , isMEInactive
       ) where

import           Control.Exception      (Exception (..), SomeException)
import           Data.Aeson.TH          (defaultOptions, deriveJSON)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Data.Text.Buildable    (Buildable (build))
import qualified Data.Text.Format       as F
import           Data.Typeable          (Typeable)

import           RSCoin.Core.Error      (rscExceptionFromException,
                                         rscExceptionToException)
import           RSCoin.Core.Types      (PeriodId)


import           Data.MessagePack       (MessagePack (fromObject, toObject))
import           Data.MessagePack.Aeson (AsMessagePack (AsMessagePack),
                                         getAsMessagePack)



data MintetteError
    = MEInternal Text                     -- ^ Should not happen.
    | MEInactive                          -- ^ Mintette is not active right now.
    | MEPeriodMismatch PeriodId PeriodId  -- ^ PeriodId expected by mintette is
                                          -- different from the one expected by somebody.
    | MEInvalidTxSums                     -- ^ Mintette received transaction with invalid sums.
    | MEInconsistentRequest Text          -- ^ Inconsistency detected.
    | MEDoubleSpending                    -- ^ Double spending detected.
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
    build MEInvalidTxSums = "sum of transaction outputs is greater than sum of inputs"
    build (MEInconsistentRequest msg) = build msg
    build MEDoubleSpending = "most likely double spending takes place"
    build MEInvalidSignature = "failed to verify signature"
    build MENotConfirmed = "transaction doesn't have enough confirmations"
    build MEAlreadyActive = "can't start new period when period is active"

$(deriveJSON defaultOptions ''MintetteError)

instance MessagePack MintetteError where
    toObject = toObject . AsMessagePack
    fromObject = fmap getAsMessagePack . fromObject

isMEInactive :: SomeException -> Bool
isMEInactive = maybe False (== MEInactive) . fromException
