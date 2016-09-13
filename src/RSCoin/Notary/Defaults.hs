-- | Module contains predefined constants for Notary configuration.

module RSCoin.Notary.Defaults
       ( allocationAttemptsLimit
       , defaultAllocationEndurance
       , defaultTransactionEndurance
       ) where

-- | Maximum allowed number of attempts to allocate MS address per period.
allocationAttemptsLimit :: Int
allocationAttemptsLimit = 100500

-- | Time interval in periods to keep alive allocation requests (~2 hours)
defaultAllocationEndurance :: Int
defaultAllocationEndurance = 72

-- | Time interval in periods to keep alive transactions (~20 minutes)
defaultTransactionEndurance :: Int
defaultTransactionEndurance = 12
