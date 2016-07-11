-- | All possible runtime errors in Notary.

module RSCoin.Notary.Error
       ( NotaryError (..)
       ) where

import           Control.Exception       (Exception (..))
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
    | NEStrategyNotSupported Text  -- ^ Address's strategy is not supported, with name provided
    | NEUnrelatedSignature         -- ^ Signature provided doesn't correspond to any of address' parties
    | NEInvalidSignature           -- ^ Invalid signature provided
    | NEInvalidChain
    | NEBlocked
    deriving (Eq, Show, Typeable)

instance Exception NotaryError where
    toException   = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable NotaryError where
    build NEAddrNotRelativeToTx      = "NEAddrNotRelativeToTx"
    build (NEAddrIdNotInUtxo pId)    = bprint ("NEAddrIdNotInUtxo, notary's periodId " % int) pId
    build (NEStrategyNotSupported s) = bprint ("NEStrategyNotSupported, strategy " % stext) s
    build NEUnrelatedSignature       = "NEUnrelatedSignature"
    build NEInvalidSignature         = "NEInvalidSignature"
    build NEInvalidChain         = "NEInvalidChain"
    build NEBlocked         = "NEBlocked"

toObj
    :: MessagePack a
    => (Int, a) -> Object
toObj = toObject

instance MessagePack NotaryError where
    toObject NEAddrNotRelativeToTx      = toObj (0, ())
    toObject (NEAddrIdNotInUtxo pId)    = toObj (1, pId)
    toObject (NEStrategyNotSupported s) = toObj (2, s)
    toObject NEUnrelatedSignature       = toObj (3, ())
    toObject NEInvalidSignature         = toObj (4, ())
    toObject NEInvalidChain = toObj (5,())
    toObject NEBlocked = toObj (6, ())

    fromObject obj = do
        (i, payload) <- fromObject obj
        case (i :: Int) of
            0 -> pure NEAddrNotRelativeToTx
            1 -> NEAddrIdNotInUtxo      <$> fromObject payload
            2 -> NEStrategyNotSupported <$> fromObject payload
            3 -> pure NEUnrelatedSignature
            4 -> pure NEInvalidSignature
            5 -> pure NEInvalidChain
            6 -> pure NEBlocked
            _ -> Nothing
