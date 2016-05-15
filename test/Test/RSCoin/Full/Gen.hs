{-# LANGUAGE ScopedTypeVariables #-}

-- | Arbitrary instances for full testing.

module Test.RSCoin.Full.Gen
       ( genValidActions
       ) where

import           Control.Monad                   (replicateM)
import           Control.Monad.State             (StateT, evalStateT)
import           Control.Monad.Trans             (lift)
import           Data.Time.Units                 (addTime)
import           Test.QuickCheck                 (Arbitrary (arbitrary), Gen,
                                                  NonNegative (..), choose,
                                                  sized)

import qualified RSCoin.Core                     as C
import           RSCoin.Timed                    (Microsecond, minute)

import           Test.RSCoin.Core.Arbitrary      ()
import           Test.RSCoin.Full.Action         (PartToSend (..),
                                                  SomeAction (SomeAction),
                                                  UserAction (..),
                                                  WaitAction (..))
import           Test.RSCoin.Full.Context        (MintetteNumber, UserNumber,
                                                  bankUserAddressesCount,
                                                  userAddressesCount)
import           Test.RSCoin.Full.Initialization (InitAction (InitAction))
import           Test.RSCoin.Timed.Arbitrary     ()

instance Arbitrary MintetteNumber where
    arbitrary = pure 1

instance Arbitrary UserNumber where
    arbitrary = pure 1

instance Arbitrary PartToSend where
    arbitrary = PartToSend <$> choose (0.001, 1.0)

genWaitAction :: a -> Gen (WaitAction a)
genWaitAction a = WaitAction <$> arbitrary <*> pure a

type BalancesList = [C.Coin]

data AllBalances = AllBalances
    { bankBalances  :: BalancesList
    , usersBalances :: [BalancesList]
    } deriving (Show)

initialBalances :: UserNumber -> AllBalances
initialBalances userNumber =
    AllBalances
    { bankBalances = C.genesisValue : replicate (bankUserAddressesCount - 1) 0
    , usersBalances = replicate (fromIntegral userNumber) $
      replicate userAddressesCount 0
    }

-- TODO
genValidFormTransaction :: StateT AllBalances Gen UserAction
genValidFormTransaction =
    lift $ FormTransaction <$> arbitrary <*> arbitrary <*> arbitrary

genUpdateBlockchain :: Gen UserAction
genUpdateBlockchain = UpdateBlockchain <$> arbitrary

type ActionsDescription = ([SomeAction], Microsecond)

-- | Generate sequence of action which can be applied to empty context
-- (created using mkTestContext) and are guaranteed to be executed
-- without fails.
genValidActions :: UserNumber -> Gen ActionsDescription
genValidActions userNumber = do
    userActions <- map SomeAction <$> sized genUserActions
    actions <- mapM genWaitAction userActions
    let actionsRunningTime = sum $ map runningTime actions
        safeRunningTime = addTime actionsRunningTime (minute 1)
    return (SomeAction InitAction : map SomeAction actions, safeRunningTime)
  where
    runningTime (WaitAction t _) = getNonNegative t
    genUserActions s =
        let updates = s `div` 10
            transactions = s - updates
        in (++) <$> replicateM updates genUpdateBlockchain <*>
           evalStateT
               (replicateM transactions genValidFormTransaction)
               (initialBalances userNumber)
