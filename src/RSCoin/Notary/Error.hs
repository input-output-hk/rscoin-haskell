-- | All possible runtime errors in Notary.

module RSCoin.Notary.Error
       ( NotaryError (..)
       ) where

import           Control.Exception       (Exception (..))

import           Data.Data               (Data)
import           Data.MessagePack        (MessagePack (fromObject, toObject),
                                          Object)
import           Data.Text               (Text)
import           Data.Text.Buildable     (Buildable (build))
import           Data.Typeable           (Typeable)

import           Formatting              (bprint, int, stext, (%))

import           RSCoin.Core.Error       (rscExceptionFromException,
                                          rscExceptionToException)
import           RSCoin.Core.MessagePack ()
import           RSCoin.Core.Types       (PeriodId)

data NotaryError
    = NEAddrNotRelativeToTx        -- ^ Address doesn't correspond to any of transaction's inputs
    | NEAddrIdNotInUtxo PeriodId   -- ^ One of transaction's addrId is not present in utxo
                                   --   PeriodId supplied -- actual periodId known to Notary
    | NEBlocked                    -- ^ User has reached limit number of attempts for multisig allocation
    | NEInvalidArguments Text      -- ^ Generic exception for invalid arguments
    | NEInvalidSignature           -- ^ Invalid signature provided
    | NEStrategyNotSupported Text  -- ^ Address's strategy is not supported, with name provided
    | NEUnrelatedSignature Text    -- ^ Signature provided doesn't correspond to any of address' parties
    deriving (Eq, Show, Typeable, Data)

instance Exception NotaryError where
    toException   = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable NotaryError where
    build NEAddrNotRelativeToTx      = "NEAddrNotRelativeToTx"
    build (NEAddrIdNotInUtxo pId)    = bprint ("NEAddrIdNotInUtxo, notary's periodId " % int) pId
    build NEBlocked                  = "NEBlocked"
    build (NEInvalidArguments msg)   = bprint ("NEInvalidArguments: " % stext) msg
    build NEInvalidSignature         = "NEInvalidSignature"
    build (NEStrategyNotSupported s) = bprint ("NEStrategyNotSupported, strategy " % stext) s
    build (NEUnrelatedSignature msg) = bprint ("NEUnrelatedSignature: " % stext) msg

toObj
    :: MessagePack a
    => (Int, a) -> Object
toObj = toObject

instance MessagePack NotaryError where
    toObject NEAddrNotRelativeToTx      = toObj (0, ())
    toObject (NEAddrIdNotInUtxo pId)    = toObj (1, pId)
    toObject NEBlocked                  = toObj (2, ())
    toObject (NEInvalidArguments msg)   = toObj (3, msg)
    toObject NEInvalidSignature         = toObj (4, ())
    toObject (NEStrategyNotSupported s) = toObj (5, s)
    toObject (NEUnrelatedSignature msg) = toObj (6, msg)

    fromObject obj = do
        (i, payload) <- fromObject obj
        case (i :: Int) of
            0 -> pure NEAddrNotRelativeToTx
            1 -> NEAddrIdNotInUtxo      <$> fromObject payload
            2 -> pure NEBlocked
            3 -> NEInvalidArguments     <$> fromObject payload
            4 -> pure NEInvalidSignature
            5 -> NEStrategyNotSupported <$> fromObject payload
            6 -> NEUnrelatedSignature   <$> fromObject payload
            _ -> Nothing
