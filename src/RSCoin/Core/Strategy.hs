-- | Strategy-related functions/helpers

module RSCoin.Core.Strategy
     ( ifStrategyCompleted
     ) where

import           RSCoin.Core.Crypto.Signing (Signature, verify)
import           RSCoin.Core.Primitives     (Address (getAddress), Transaction)
import           RSCoin.Core.Types          (Strategy (..))
import qualified Data.Set               as S

-- | Checks if the inner state of strategy allows us to send
-- transaction and it will be accepted
ifStrategyCompleted :: Strategy -> Address -> [(Address, Signature)] -> Transaction -> Bool
ifStrategyCompleted DefaultStrategy address signs tx =
    any (\(addr,sign) -> address == addr &&
                         verify (getAddress address) sign tx) signs
ifStrategyCompleted (MOfNStrategy m addresses) _ signs tx =
    let hasSignature address =
            any (\(addr,sign) -> address == addr &&
                                 verify (getAddress address) sign tx)
                signs
        withSignatures = S.filter hasSignature addresses
    in length withSignatures >= m
