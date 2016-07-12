-- | Strategy-related functions/helpers

module RSCoin.Core.Strategy
     ( isStrategyCompleted
     ) where

import qualified Data.Set                   as S

import           RSCoin.Core.Crypto.Signing (Signature)
import           RSCoin.Core.Primitives     (Address, Transaction)
import           RSCoin.Core.Transaction    (validateSignature)
import           RSCoin.Core.Types          (Strategy (..))

-- | Checks if the inner state of strategy allows us to send
-- transaction and it will be accepted
isStrategyCompleted :: Strategy -> Address -> [(Address, Signature)] -> Transaction -> Bool
isStrategyCompleted DefaultStrategy address signs tx =
    any (\(addr, signature) -> address == addr &&
                         validateSignature signature addr tx) signs
isStrategyCompleted (MOfNStrategy m addresses) _ signs tx =
    let hasSignature address =
            any (\(addr, signature) -> address == addr &&
                                 validateSignature signature addr tx)
                signs
        withSignatures = S.filter hasSignature addresses
    in length withSignatures >= m
