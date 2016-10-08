{-# LANGUAGE TemplateHaskell #-}

-- | Runtime environment.

module RSCoin.Mintette.Env
       ( RuntimeEnv

       , mkRuntimeEnv

       , reActionLogsLimit
       , reSecretKey
       , rePermittedAddrs
       ) where

import           Control.Lens  (makeLenses)
import           Data.SafeCopy (base, deriveSafeCopy)

import           RSCoin.Core   (Address, SecretKey)

data RuntimeEnv = RuntimeEnv
    { _reActionLogsLimit :: !Word         -- ^ Maximum number of
                                          -- periods for which action
                                          -- logs are stored.
    , _reSecretKey       :: !SecretKey    -- ^ Secret key of mintette.
    , _rePermittedAddrs  :: ![Address]
    }

$(makeLenses ''RuntimeEnv)
$(deriveSafeCopy 0 'base ''RuntimeEnv)

mkRuntimeEnv :: Word -> SecretKey -> [Address] -> RuntimeEnv
mkRuntimeEnv = RuntimeEnv
