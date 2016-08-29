{-# LANGUAGE TemplateHaskell #-}

-- | Bank-specific types.

module RSCoin.Bank.Types
       ( EmissionId
       , HBlockMetadata (..)
       ) where

import           Data.SafeCopy         (base, deriveSafeCopy)
import           Data.Time.Clock.POSIX (POSIXTime)

import qualified RSCoin.Core           as C

-- | Emission is identified by transaction hash.
type EmissionId = C.TransactionId

data HBlockMetadata = HBlockMetadata
    { hbmTimestamp :: !POSIXTime
    , hbmEmission  :: !EmissionId
    } deriving (Show)

$(deriveSafeCopy 0 'base ''HBlockMetadata)
