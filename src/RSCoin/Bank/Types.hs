{-# LANGUAGE TemplateHaskell #-}

-- | Bank-specific types.

module RSCoin.Bank.Types
       ( EmissionId
       , HBlockMetadata (..)
       ) where

import           Data.SafeCopy         (base, deriveSafeCopy)
import           Data.Time.Clock.POSIX (POSIXTime)

import           Serokell.Data.Variant (ToVariant (toVariant),
                                        Variant (VarFloat, VarString), varMap)
import           Serokell.Util.Text    (show')

import qualified RSCoin.Core           as C

-- | Emission is identified by transaction hash.
type EmissionId = C.TransactionId

data HBlockMetadata = HBlockMetadata
    { hbmTimestamp :: !POSIXTime
    , hbmEmission  :: !EmissionId
    } deriving (Show)

instance ToVariant HBlockMetadata where
    toVariant HBlockMetadata{..} =
        varMap
            [ ("timestamp", VarFloat (realToFrac hbmTimestamp))
            , ("emission", VarString (show' hbmEmission))]

$(deriveSafeCopy 0 'base ''HBlockMetadata)
