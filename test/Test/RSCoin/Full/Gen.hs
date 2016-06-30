{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Arbitrary instances for full testing.

module Test.RSCoin.Full.Gen
       ( genValidActions
       ) where

import           Control.Lens                    (Traversal', ix, makeLenses,
                                                  use, (%=))
import           Control.Monad                   (replicateM)
import           Control.Monad.State             (MonadState, StateT,
                                                  evalStateT)
import           Control.Monad.Trans             (lift)
import           Data.Either.Combinators         (mapRight)
import           Data.List                       (genericIndex, genericLength,
                                                  genericReplicate)
import qualified Data.Map                        as M
import           Data.Maybe                      (catMaybes)
import           Data.Time.Units                 (fromMicroseconds)
import           Test.QuickCheck                 (Arbitrary (arbitrary), Gen,
                                                  NonEmptyList (..), choose,
                                                  listOf, oneof, sized,
                                                  sublistOf, suchThat, vectorOf)

import qualified RSCoin.Core                     as C

import           Test.RSCoin.Core.Arbitrary      ()
import           Test.RSCoin.Full.Action         (Coloring (Coloring),
                                                  PartToSend (..), PartsToSend,
                                                  SomeAction (SomeAction),
                                                  ToAddress, UserAction (..),
                                                  UserIndex, WaitAction (..),
                                                  applyPartsToSend)
import           Test.RSCoin.Full.Constants      (maxColor, minColor)
import           Test.RSCoin.Full.Context        (MintetteNumber, UserNumber)
import           Test.RSCoin.Full.Initialization (bankUserAddressesCount,
                                                  userAddressesCount)
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
genWaitAction a =
    WaitAction <$> (fromMicroseconds <$> choose (0, 5 * 1000 * 1000)) <*>
    pure a -- at most 5 seconds

genColor :: Gen C.Color
genColor = choose (minColor, maxColor)

instance Arbitrary Coloring where
    arbitrary = do
        colors <- listOf (genColor `suchThat` (/= 0))
        parts <- vectorOf (length colors) (choose (1.0e-4, 1.0))
        targetSum <- choose (0.1, 1.0)
        let s = sum parts
            multiplier = targetSum / s
        return .
            Coloring . M.fromListWith (+) . zip colors . map (* multiplier) $
            parts

genToAddress :: UserNumber -> Gen ToAddress
genToAddress (fromIntegral -> userNumber) = mapRight fixIndices <$> arbitrary
  where
    fixIndices (usrIdx,addrIdx) =
        ( (`mod` userNumber) <$> usrIdx
        , maybe
              (`mod` bankUserAddressesCount)
              (const (`mod` userAddressesCount))
              usrIdx
              addrIdx)

-- | I-th element in this list stores CoinsMap for `i-th` address.
type BalancesList = [C.CoinsMap]

data AllBalances = AllBalances
    { _bankBalances  :: BalancesList
    , _usersBalances :: [BalancesList]
    } deriving (Show)

$(makeLenses ''AllBalances)

balancesLens :: UserIndex -> Traversal' AllBalances BalancesList
balancesLens =
    maybe
        bankBalances
        (\i ->
              usersBalances . ix (fromIntegral i))

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
genValidSubmitTransaction :: StateT AllBalances Gen (Maybe UserAction)
genValidSubmitTransaction = do
    bb <- (Nothing, ) <$> use bankBalances
    ub <- zip (fmap Just [0 ..]) <$> use usersBalances
    genValidSubmitTransactionDo .
        filter (not . null . C.coinsToList . C.mergeCoinsMaps . snd) $
        bb : ub

genValidSubmitTransactionDo :: [(UserIndex, BalancesList)]
                            -> StateT AllBalances Gen (Maybe UserAction)
genValidSubmitTransactionDo [] = return Nothing
genValidSubmitTransactionDo solvents =
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
       userNumber <- genericLength <$> use usersBalances
       dest <- lift $ genToAddress userNumber
       addToDestination uIdx fromAddresses dest
       SubmitTransaction uIdx (NonEmpty fromAddresses) dest <$> lift arbitrary
  where
    nonEmptySublistOf :: [a] -> Gen [a]
    nonEmptySublistOf xs = sublistOf xs `suchThat` (not . null)
    subtractFromInput usrIndex usrAddrIndex parts =
        balancesLens usrIndex . ix (fromIntegral usrAddrIndex) %=
        decreaseBalance parts
    decreaseBalance :: PartsToSend -> C.CoinsMap -> C.CoinsMap
    decreaseBalance parts coinsMap =
        C.subtractCoinsMap coinsMap $ applyPartsToSend parts coinsMap
    addToDestination
        :: MonadState AllBalances m
        => UserIndex -> [(Word, PartsToSend)] -> ToAddress -> m ()
    addToDestination _ _ (Left _) = return ()
    addToDestination srcUsrIdx inputs (Right (dstUsrIdx,dstUsrAddrIdx)) = do
        balances <- use $ balancesLens srcUsrIdx
        let step (addrIdx,parts) =
                applyPartsToSend parts $ balances `genericIndex` addrIdx
            dstLens = balancesLens dstUsrIdx . ix (fromIntegral dstUsrAddrIdx)
            maps = map step inputs
        mapM_
            (\summand ->
                  dstLens %= C.addCoinsMap summand)
            maps

genUpdateBlockchain :: Gen UserAction
genUpdateBlockchain = UpdateBlockchain <$> arbitrary

type ActionsDescription = [SomeAction]

-- | Generate sequence of actions which can be applied to empty context
-- (created using mkTestContext) and are guaranteed to be executed
-- without fails.
genValidActions :: UserNumber -> Gen ActionsDescription
genValidActions userNumber = do
    userActions <- map SomeAction <$> sized genUserActions
    actions <- mapM genWaitAction userActions
    return (map SomeAction actions)
  where
    genUserActions s =
        let updates = s `div` 10
            transactions = s - updates
        in (++) <$> replicateM updates genUpdateBlockchain <*>
           (catMaybes <$>
            evalStateT
                (replicateM transactions genValidSubmitTransaction)
                (initialBalances userNumber))
