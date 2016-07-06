-- | All possible runtime errors in Mintette

module RSCoin.Signer.Error
       ( SignerError (..)
       ) where

import           Control.Exception       (Exception (..))
import           Data.MessagePack        (MessagePack (fromObject, toObject),
                                          Object)
import           Data.Text.Buildable     (Buildable (build))
import qualified Data.Text.Format        as F
import           Data.Typeable           (Typeable)

import           RSCoin.Core.Error       (rscExceptionFromException,
                                          rscExceptionToException)
import           RSCoin.Core.MessagePack ()
import           RSCoin.Core.Types       (PeriodId)

data SignerError
    = SEAddrNotRelativeToTx -- ^ Address doesn't correspond to any of transaction's inputs
    | SEAddrIdNotInUtxo PeriodId -- ^ One of transaction's addrId is not present in utxo
                                 --   PeriodId supplied -- actual periodId known to Signer
    | SEStrategyNotSupported String -- ^ Address's strategy is not supported, with name provided
    | SEUnrelatedSignature -- ^ Signature provided doesn't correspond to any of address' parties
    | SEInvalidSignature -- ^ Invalid signature provided
    deriving (Show, Typeable, Eq)

instance Exception SignerError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable SignerError where
    build SEAddrNotRelativeToTx = "SEAddrNotRelativeToTx"
    build (SEAddrIdNotInUtxo pId) = F.build "SEAddrIdNotInUtxo, signer's periodId {}" $ F.Only pId
    build (SEStrategyNotSupported s) = F.build "SEStrategyNotSupported, strategy {}" s
    build SEUnrelatedSignature = "SEUnrelatedSignature"
    build SEInvalidSignature = "SEInvalidSignature"

toObj
    :: MessagePack a
    => (Int, a) -> Object
toObj = toObject

instance MessagePack SignerError where
    toObject SEAddrNotRelativeToTx = toObj (0, ())
    toObject (SEAddrIdNotInUtxo pId) = toObj (1, pId)
    toObject (SEStrategyNotSupported s) = toObj (2, s)
    toObject SEUnrelatedSignature = toObj (3, ())
    toObject SEInvalidSignature = toObj (4, ())
    fromObject obj = do
        (i,payload) <- fromObject obj
        case (i :: Int) of
            0 -> pure SEAddrNotRelativeToTx
            1 -> SEAddrIdNotInUtxo <$> fromObject payload
            2 -> SEStrategyNotSupported <$> fromObject payload
            3 -> pure SEUnrelatedSignature
            4 -> pure SEInvalidSignature
            _ -> Nothing
