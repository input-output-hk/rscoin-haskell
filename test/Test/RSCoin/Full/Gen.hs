{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | Arbitrary instances for full testing.

module Test.RSCoin.Full.Gen
       ( extraRunningTime
       , genValidActions
       ) where

import           Control.Lens                    (Traversal', ix, makeLenses,
                                                  use, (%=))
import           Control.Monad                   (replicateM)
import           Control.Monad.State             (MonadState, StateT,
                                                  evalStateT)
import           Control.Monad.Trans             (lift)
import           Data.List                       (genericIndex,
                                                  genericReplicate)
import qualified Data.Map                        as M
import           Data.Maybe                      (catMaybes)
import           Data.Time.Units                 (addTime)
import           Test.QuickCheck                 (Arbitrary (arbitrary), Gen,
                                                  NonEmptyList (..),
                                                  NonNegative (..), choose,
                                                  oneof, sized, sublistOf)

import qualified RSCoin.Core                     as C
import           RSCoin.Timed                    (Microsecond, Second)

import           Test.RSCoin.Core.Arbitrary      ()
import           Test.RSCoin.Full.Action         (PartToSend (..), PartsToSend,
                                                  SomeAction (SomeAction),
                                                  ToAddress, UserAction (..),
                                                  UserIndex, WaitAction (..),
                                                  applyPartsToSend)
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

genPartsToSend :: C.CoinsMap -> Gen PartsToSend
genPartsToSend =
    fmap M.fromList .
    mapM
        (\color ->
              (color, ) <$> arbitrary) .
    M.keys

genWaitAction :: a -> Gen (WaitAction a)
genWaitAction a = WaitAction <$> arbitrary <*> pure a

-- | I-th element in this list stores CoinsMap for `i-th` address.
type BalancesList = [C.CoinsMap]

data AllBalances = AllBalances
    { _bankBalances  :: BalancesList
    , _usersBalances :: [BalancesList]
    } deriving (Show)

$(makeLenses ''AllBalances)

zeroBalance :: C.CoinsMap
zeroBalance = M.empty

genesisBalance :: C.CoinsMap
genesisBalance = C.coinsToMap [C.genesisValue]

initialBalances :: UserNumber -> AllBalances
initialBalances userNumber =
    AllBalances
    { _bankBalances = genesisBalance :
      replicate (bankUserAddressesCount - 1) zeroBalance
    , _usersBalances = genericReplicate userNumber $
      replicate userAddressesCount zeroBalance
    }

-- It may be impossible if there were transactions to arbitrary
-- address (in this case there may be effectively no coins in system).
genValidFormTransaction :: StateT AllBalances Gen (Maybe UserAction)
genValidFormTransaction = do
    bb <- (Nothing, ) <$> use bankBalances
    ub <- zip (fmap Just [0 ..]) <$> use usersBalances
    genValidFormTransactionDo .
        filter (not . null . C.coinsToList . C.mergeCoinsMaps . snd) $
        bb : ub

genValidFormTransactionDo :: [(UserIndex, BalancesList)]
                          -> StateT AllBalances Gen (Maybe UserAction)
genValidFormTransactionDo [] = return Nothing
genValidFormTransactionDo solvents =
    Just <$>
    do (uIdx,uBalances) <- lift . oneof . map pure $ solvents
       let nonEmptyAddresses =
               map fst . filter (not . M.null . snd) . zip [0 ..] $ uBalances
       indicesToUse <- lift $ nonEmptySublistOf nonEmptyAddresses
       fromAddresses <-
           mapM
               (\i ->
                     (i, ) <$>
                     lift (genPartsToSend $ uBalances `genericIndex` i))
               indicesToUse
       mapM_ (uncurry $ subtractFromInput uIdx) fromAddresses
       dest <- lift arbitrary
       addToDestination uIdx fromAddresses dest
       return $ FormTransaction uIdx (NonEmpty fromAddresses) dest
  where
    nonEmptySublistOf :: [a] -> Gen [a]
    nonEmptySublistOf xs = do
        res <- sublistOf xs
        case res of
            [] -> (: []) <$> (oneof . map pure $ xs)
            _ -> return res
    subtractFromInput usrIndex usrAddrIndex parts = do
        let balancesLens :: Traversal' AllBalances BalancesList
            balancesLens =
                maybe
                    bankBalances
                    (\i ->
                          usersBalances . ix (fromIntegral i))
                    usrIndex
        balancesLens . ix (fromIntegral usrAddrIndex) %= decreaseBalance parts
    decreaseBalance :: PartsToSend -> C.CoinsMap -> C.CoinsMap
    decreaseBalance parts coinsMap =
        C.subtractCoinsMap coinsMap $ applyPartsToSend parts coinsMap
    addToDestination
        :: MonadState AllBalances m
        => UserIndex -> [(Word, PartsToSend)] -> ToAddress -> m ()
    addToDestination _ _ (Left _) = return ()
    addToDestination srcUsrIdx inputs (Right (dstUsrIdx,dstUsrAddrIdx)) = do
        balances <-
            use $
            maybe
                bankBalances
                (\i ->
                      usersBalances . ix (fromIntegral i))
                srcUsrIdx
        let step (addrIdx,parts) = applyPartsToSend parts $ balances `genericIndex` addrIdx
            dstLens =
                maybe
                    bankBalances
                    (\i ->
                          usersBalances . ix (fromIntegral i))
                    dstUsrIdx .
                ix (fromIntegral dstUsrAddrIdx)
            maps = map step inputs
        mapM_
            (\summand ->
                  dstLens %= C.addCoinsMap summand)
            maps

genUpdateBlockchain :: Gen UserAction
genUpdateBlockchain = UpdateBlockchain <$> arbitrary

type ActionsDescription = ([SomeAction], Microsecond)

extraRunningTime :: Second
extraRunningTime = 10

-- | Generate sequence of actions which can be applied to empty context
-- (created using mkTestContext) and are guaranteed to be executed
-- without fails.
genValidActions :: UserNumber -> Gen ActionsDescription
genValidActions userNumber = do
    userActions <- map SomeAction <$> sized genUserActions
    actions <- mapM genWaitAction userActions
    let actionsRunningTime = sum $ map runningTime actions
        safeRunningTime = actionsRunningTime `addTime` extraRunningTime
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
