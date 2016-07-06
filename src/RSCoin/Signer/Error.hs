-- | All possible runtime errors in Mintette

module RSCoin.Signer.Error
       ( SignerError (..)
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

data SignerError
    = SEAddrNotRelativeToTx
    deriving (Show, Typeable, Eq)

instance Exception SignerError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable SignerError where
    build SEAddrNotRelativeToTx = "can't start new period when period is active"

toObj
    :: MessagePack a
    => (Int, a) -> Object
toObj = toObject

instance MessagePack SignerError where
    toObject SEAddrNotRelativeToTx = toObj (0, ())
    fromObject obj = do
        (i,payload) <- fromObject obj
        case (i :: Int) of
            0 -> pure SEAddrNotRelativeToTx
            _ -> Nothing
