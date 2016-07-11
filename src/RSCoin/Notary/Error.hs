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
    | NEBlocked                    -- ^ User has reached limit number of attempts for multisig allocation
    | NEInvalidChain               -- ^ Invalid chain of certificates provided
    | NEInvalidSignature           -- ^ Invalid signature provided
    | NEStrategyNotSupported Text  -- ^ Address's strategy is not supported, with name provided
    | NEUnrelatedSignature         -- ^ Signature provided doesn't correspond to any of address' parties
    deriving (Eq, Show, Typeable)

instance Exception NotaryError where
    toException   = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable NotaryError where
    build NEAddrNotRelativeToTx      = "NEAddrNotRelativeToTx"
    build (NEAddrIdNotInUtxo pId)    = bprint ("NEAddrIdNotInUtxo, notary's periodId " % int) pId
    build NEBlocked                  = "NEBlocked"
    build NEInvalidChain             = "NEInvalidChain"
    build NEInvalidSignature         = "NEInvalidSignature"
    build (NEStrategyNotSupported s) = bprint ("NEStrategyNotSupported, strategy " % stext) s
    build NEUnrelatedSignature       = "NEUnrelatedSignature"

toObj
    :: MessagePack a
    => (Int, a) -> Object
toObj = toObject

instance MessagePack NotaryError where
    toObject NEAddrNotRelativeToTx      = toObj (0, ())
    toObject (NEAddrIdNotInUtxo pId)    = toObj (1, pId)
    toObject NEBlocked                  = toObj (2, ())
    toObject NEInvalidChain             = toObj (3, ())
    toObject NEInvalidSignature         = toObj (4, ())
    toObject (NEStrategyNotSupported s) = toObj (5, s)
    toObject NEUnrelatedSignature       = toObj (6, ())

    fromObject obj = do
        (i, payload) <- fromObject obj
        case (i :: Int) of
            0 -> pure NEAddrNotRelativeToTx
            1 -> NEAddrIdNotInUtxo      <$> fromObject payload
            2 -> pure NEBlocked
            3 -> pure NEInvalidChain
            4 -> pure NEInvalidSignature
            5 -> NEStrategyNotSupported <$> fromObject payload
            6 -> pure NEUnrelatedSignature
            _ -> Nothing
