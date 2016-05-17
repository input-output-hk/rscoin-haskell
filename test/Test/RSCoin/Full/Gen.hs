{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

-- | Arbitrary instances for full testing.

module Test.RSCoin.Full.Gen
       ( genValidActions
       ) where

import           Control.Lens                    (ix, makeLenses, use, (.=))
import           Control.Monad                   (forM_, replicateM)
import           Control.Monad.State             (StateT, evalStateT)
import           Control.Monad.Trans             (lift)
import           Data.List                       (genericIndex)
import           Data.Maybe                      (catMaybes)
import           Data.Time.Units                 (addTime)
import           Test.QuickCheck                 (Arbitrary (arbitrary), Gen,
                                                  NonEmptyList (..),
                                                  NonNegative (..), choose,
                                                  oneof, sized, sublistOf)

import qualified RSCoin.Core                     as C
import           RSCoin.Timed                    (Microsecond, minute)

import           Test.RSCoin.Core.Arbitrary      ()
import           Test.RSCoin.Full.Action         (PartToSend (..),
                                                  SomeAction (SomeAction),
                                                  UserAction (..), UserIndex,
                                                  WaitAction (..),
                                                  applyPartToSend)
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
    { _bankBalances  :: BalancesList
    , _usersBalances :: [BalancesList]
    } deriving (Show)

$(makeLenses ''AllBalances)

initialBalances :: UserNumber -> AllBalances
initialBalances userNumber =
    AllBalances
    { _bankBalances = C.genesisValue : replicate (bankUserAddressesCount - 1) 0
    , _usersBalances = replicate (fromIntegral userNumber) $
      replicate userAddressesCount 0
    }

-- It may be impossible if there were transactions to arbitrary
-- address (in this case there may be effectively no coins in system).
genValidFormTransaction :: StateT AllBalances Gen (Maybe UserAction)
genValidFormTransaction = do
    bb <- (Nothing, ) <$> use bankBalances
    ub <- zip (fmap Just [0 ..]) <$> use usersBalances
    genValidFormTransactionDo $ filter ((> 0) . sum . snd) $ bb : ub

genValidFormTransactionDo :: [(UserIndex, BalancesList)]
                          -> StateT AllBalances Gen (Maybe UserAction)
genValidFormTransactionDo [] = return Nothing
genValidFormTransactionDo solvents =
    Just <$>
    do (uIdx,uAddresses) <- lift . oneof . map pure $ solvents
       let nonEmptyAddresses = map fst . zip [0 ..] . filter (> 0) $ uAddresses
       indicesToUse <- lift $ nonEmptySublistOf nonEmptyAddresses
       fromAddresses <-
           mapM
               (\i ->
                     (i, ) <$> lift arbitrary)
               indicesToUse
       forM_ fromAddresses $
           \(addrIdx,p) ->
                do amountWas <-
                       (`genericIndex` addrIdx) <$>
                       maybe
                           (use bankBalances)
                           (\i ->
                                 use $ usersBalances . ix (fromIntegral i))
                           uIdx
                   let amountSent = applyPartToSend p amountWas
                       amountFinal = amountWas - amountSent
                   maybe
                       (bankBalances . ix (fromIntegral addrIdx) .= amountFinal)
                       (\i ->
                             usersBalances . ix (fromIntegral i) .
                             ix (fromIntegral addrIdx) .=
                             amountFinal)
                       uIdx
       lift $ FormTransaction uIdx (NonEmpty fromAddresses) <$> arbitrary
  where
    nonEmptySublistOf :: [a] -> Gen [a]
    nonEmptySublistOf xs = do
        res <- sublistOf xs
        case res of
            [] -> (: []) <$> (oneof . map pure $ xs)
            _ -> return res

genUpdateBlockchain :: Gen UserAction
genUpdateBlockchain = UpdateBlockchain <$> arbitrary

type ActionsDescription = ([SomeAction], Microsecond)

-- | Generate sequence of actions which can be applied to empty context
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
           (catMaybes <$>
            evalStateT
                (replicateM transactions genValidFormTransaction)
                (initialBalances userNumber))
