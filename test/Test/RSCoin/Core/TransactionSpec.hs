{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | RSCoin.Core.Transaction specification

module Test.RSCoin.Core.TransactionSpec
       ( spec,
       ) where

import           Control.Lens               (view, _3)
import           Data.Bifunctor             (first, second)
import qualified Data.IntMap.Strict         as M (IntMap, elems,
                                                  findWithDefault, foldrWithKey,
                                                  lookup, mapWithKey, null, (!))
import           Data.List                  (genericLength, sort)
import           Data.Maybe                 (isJust)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen,
                                             NonEmptyList (..), choose, vector)

import qualified RSCoin.Core                as C

import           Test.RSCoin.Core.Arbitrary ()

newtype TransactionValid = TransactionValid
    { getTr :: C.Transaction
    } deriving (Show)

genRationalInRange :: Rational -> Rational -> Gen Rational
genRationalInRange lo hi =
    (toRational :: Double -> Rational) <$>
    choose (fromRational lo, fromRational hi)

genCoinInRange :: C.Color -> Rational -> Rational -> Gen C.Coin
genCoinInRange col lo hi = C.Coin col . C.CoinAmount <$> genRationalInRange lo hi

instance Arbitrary TransactionValid where
    arbitrary =
        TransactionValid <$>
        do trid <- arbitrary :: Gen C.TransactionId
           inps :: [(Int, C.Coin)] <-
               map (second abs) . getNonEmpty <$> arbitrary
           let coins :: C.CoinsMap
               inputs :: [C.AddrId]
               (coins,inputs) =
                   first C.coinsToMap $
                   unzip $
                   map
                       (\(ind,coin) ->
                             (coin, (trid, ind, coin)))
                       inps
               genOutput C.Coin{..} =
                   (,) <$>
                   arbitrary <*>
                   genCoinInRange getColor 0 (C.getAmount getCoin)
           unpaintedOutputsMap <- mapM genOutput coins
           padCols <- arbitrary :: Gen [C.Color]
           let l
                   :: Num a
                   => a
               l = genericLength padCols
               helper adr cl cn = (adr, C.Coin cl cn)
           padAddrs <- vector l :: Gen [C.Address]
           C.Transaction inputs . (M.elems unpaintedOutputsMap ++) <$>
               case M.lookup 0 unpaintedOutputsMap of
                   Nothing -> return []
                   Just (_,v) ->
                       if null padCols
                           then return []
                           else do
                               let (outpv,inpv) =
                                       (C.getCoin v, C.getCoin $ coins M.! 0)
                               v' <-
                                   C.CoinAmount <$>
                                   genRationalInRange
                                       0
                                       (C.getAmount (inpv - outpv))
                               return $
                                   zipWith3
                                       helper
                                       padAddrs
                                       padCols
                                       (repeat (v' / l))

spec :: Spec
spec =
    describe "Transaction" $ do
        describe "validateSum" $ do
            prop description_validateSumForValid validateSumCorrectForValid
            prop description_validateInputMoreOutput validateInputMoreThanOutput
            prop description_validateSumInputOutputCol validateOnlyInputColorsInOutput
        describe "validateSignature" $
            prop description_validateSignature validateSig
        describe "chooseAddresses" $ do
            prop description_chooseAddressesJustWhenPossible chooseAddressesJustWhenPossible
            prop description_chooseSmallerAddressesFirst chooseSmallerAddressesFirst
    where
      description_validateSumForValid =
        "returns true if total amount of grey coins in inputs is not less than " ++
        "amount of grey coins in outputs plus amount of coins spent to color coins"
      description_validateInputMoreOutput =
        "returns true if the amount of input coins is greater than the amount of " ++
        "output coins, false if it is smaller"
      description_validateSumInputOutputCol =
        "returns false if non-grey coins are repainted in the transaction."
      description_validateSignature =
        "returns true if the signature is issued by the public key associated " ++
        "with the address for the transaction"
      description_chooseAddressesJustWhenPossible =
        "returns Just something iff it is possible to allocate the requested " ++
        "amount of each color "
      description_chooseSmallerAddressesFirst =
        "uses addrids with smaller amount of money first"

validateSumCorrectForValid :: TransactionValid -> Bool
validateSumCorrectForValid = C.validateSum . getTr

-- | This property does the following:
-- * generate list of addrids which will serve as input;
-- * pair the coins in this list with dummy address to serve as output;
-- * create two different output lists - one with a random coin increased;
-- * and the other with that same coin decreased;
-- * check that the transaction with decreased output is validated;
-- * and that the other is not validated.

validateInputMoreThanOutput :: NonEmptyList C.AddrId -> C.Address -> Bool
validateInputMoreThanOutput (getNonEmpty -> inputs) adr =
    let outputs = map ((adr, ) . view _3) inputs
        helper [] = ([], [])
        helper ((a,C.Coin col cn):xs) =
            ((a, C.Coin col (cn + 1)) : xs, (a, C.Coin col (cn - 1)) : xs)
        (plus1,minus1) = helper outputs
        (tx1,tx2) = (C.Transaction inputs plus1, C.Transaction inputs minus1)
    in C.validateSum tx2 && (not $ C.validateSum tx1)

-- | This property does the following:
-- * generate single addrid (tid, index, Coin cl c) as input and dummy address;
-- * create output list with the dummy address and two coins:
-- * one with color cl and another with a color different from cl;
-- * each with half of the amount of the original color;
-- * check that this transaction is not validated.

validateOnlyInputColorsInOutput :: C.AddrId -> C.Address -> Bool
validateOnlyInputColorsInOutput (t, i, C.Coin col c) adr =
    let nonZero = abs col + 1
        txo = [(adr, C.Coin nonZero (c / 2)),(adr, C.Coin (nonZero + 1) (c / 2))]
    in (c == 0) ||
       (not $ C.validateSum $ C.Transaction [(t,i,C.Coin nonZero c)] txo)

validateSig :: C.SecretKey -> C.Transaction -> Bool
validateSig sk tr = C.validateSignature (C.sign sk tr) (C.Address $ C.derivePublicKey sk) tr

-- | This property will do the following:
-- * generate list of addrids and a map of colors to coins, cmap;
-- * create another coin map with the coins present in the list, adrMap;
-- * check that, for each color in cmap:
-- * the corresponding amount in adrMap is greater than the amount in cmap;
-- * if the previous is true, check that chooseAddresses returns Just ...;
-- * otherwise, check that it returns Nothing.

chooseAddressesJustWhenPossible :: NonEmptyList C.AddrId -> M.IntMap C.Coin  -> Bool
chooseAddressesJustWhenPossible (getNonEmpty -> adrlist) cmap =
    let adrCoinMap = C.coinsToMap $ map (view _3) adrlist
        step color coin accum =
            let adrcn = C.getCoin $ M.findWithDefault 0 color adrCoinMap
                coin' = C.getCoin coin
            in (adrcn - coin') >= 0 && accum
        helper col cn = C.Coin (C.Color col) (C.getCoin cn)
    in M.null cmap ||
       (M.foldrWithKey step True cmap) ==
       (isJust $ C.chooseAddresses adrlist (M.mapWithKey helper cmap))

-- | This property does the following:
-- * generate list of addrids with same color (coins are: `a1, a2, … a_n`),
-- let's say `a_m = max(a1 … a_n) + 1` and color is `c`;
-- * also add `[a1 + a_m, a2 + a_m, … a_n + a_m]`;
-- * pass these `2 * n` addrids to `chooseAddresses`;
-- * coins map is `{c: sum(a1, … a_n)}`;
-- * check that result contains exactly `a1, a2, … a_n`.
chooseSmallerAddressesFirst :: C.TransactionId -> NonEmptyList C.Coin -> Bool
chooseSmallerAddressesFirst txId (getNonEmpty -> coins0) =
    let col = C.getColor . head $ coins0
        coinsSameCol :: [C.Coin]
        coinsSameCol =
            map (\c -> c { C.getColor = col }) coins0
        maxCn = (maximum coinsSameCol) + (C.Coin col 1)
        extraCoins = map (maxCn +) coinsSameCol
        allCoins = coinsSameCol ++ extraCoins
        toAddrId :: C.Coin -> C.AddrId
        toAddrId = (txId,0,)
        addrIds, allAddrIds :: [C.AddrId]
        addrIds = map toAddrId coinsSameCol
        allAddrIds = map toAddrId allCoins
        cMap = C.coinsToMap coinsSameCol
        result = C.chooseAddresses allAddrIds cMap
        addrIdsEqual :: [C.AddrId] -> [C.AddrId] -> Bool
        addrIdsEqual l1 l2 = canonizeAddrIds l1 == canonizeAddrIds l2
        canonizeAddrIds = sort . filter ((/= 0) . C.getCoin . view _3)
    in case result of
           Nothing -> False
           Just resMap ->
               addrIdsEqual
                   (fst $ M.findWithDefault ([], undefined) (C.getC col) resMap)
                   addrIds
