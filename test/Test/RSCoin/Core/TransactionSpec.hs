{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | RSCoin.Core.Transaction specification

module Test.RSCoin.Core.TransactionSpec
       ( spec,
       ) where

import           Data.Bifunctor             (first, second)
import           Data.List                  (genericLength)
import           Data.List                  (sort)
import qualified Data.Map.Strict            as M (Map, elems, findWithDefault,
                                                  foldrWithKey, lookup,
                                                  mapWithKey, null, (!))
import           Data.Maybe                 (isJust)
import           Data.Tuple.Select          (sel3)
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
genCoinInRange col lo hi = C.Coin col <$> genRationalInRange lo hi

instance Arbitrary TransactionValid where
    arbitrary =
        TransactionValid <$>
        do trid <- arbitrary :: Gen C.Hash
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
                   (,) <$> arbitrary <*> genCoinInRange getColor 0 getCoin
           unpaintedOutputsMap <- mapM genOutput coins
           padCols <- arbitrary :: Gen [C.Color]
           let l
                   :: Num a
                   => a
               l = genericLength padCols
               helper adr cl cn = (adr, C.Coin cl cn)
           padAddrs <- vector l :: Gen [C.Address]
           C.Transaction inputs .
               (M.elems unpaintedOutputsMap ++) <$>
                 case M.lookup 0 unpaintedOutputsMap of
                     Nothing -> return []
                     Just (_,v) ->
                         if null padCols
                             then return []
                             else do let (outpv,inpv) = (C.getCoin v, C.getCoin $ coins M.! 0)
                                     v' <- genRationalInRange 0 (inpv-outpv)
                                     return $ zipWith3 helper padAddrs padCols (repeat (v' / l))

spec :: Spec
spec =
    describe "Transaction" $ do
        describe "validateSum" $ do
            prop description_validateSumForValid validateSumCorrectForValid
            prop description_validateInputLessThanOutput validateInputMoreThanOutput
            prop description_validateSumForValid validateInputMoreThanOutput2
        describe "validateSignature" $ do
            prop description_validateSignature validateSig
        describe "chooseAddresses" $ do
            prop description_chooseAddressesJustWhenPossible chooseAddressesJustWhenPossible
            prop description_chooseSmallerAddressesFirst chooseSmallerAddressesFirst
    where
      description_validateSumForValid =
        "returns true if total amount of grey coins in inputs is not less than " ++
        "amount of grey coins in outputs plus amount of coins spent to color coins"
      description_validateInputLessThanOutput =
        "returns true only if the validating function returns true on well formed " ++
        "transactions, and false on those where the total inputs are less than the " ++
        "total outputs"
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

validateInputMoreThanOutput :: NonEmptyList C.AddrId -> C.Address -> Bool
validateInputMoreThanOutput (getNonEmpty -> inputs) adr =
    let outputs = map ((adr, ) . sel3) inputs
        helper [] = ([], [])
        helper ((a,C.Coin col cn):xs) =
            ((a, C.Coin col (cn + 1)) : xs, (a, C.Coin col (cn - 1)) : xs)
        (plus1,minus1) = helper outputs
        (tx1,tx2) = (C.Transaction inputs plus1, C.Transaction inputs minus1)
    in C.validateSum tx2 && (not $ C.validateSum tx1)

validateInputMoreThanOutput2 :: C.AddrId -> Rational -> C.Address -> Bool
validateInputMoreThanOutput2 txi@(_, _, C.Coin col c) r adr =
    let (mx, mn) = (max c r, min c r)
        other = mx - mn
        txo = [(adr, C.Coin col mn),(adr, C.Coin col other)]
    in C.validateSum $ C.Transaction [txi] txo

validateSig :: C.SecretKey -> C.Transaction -> Bool
validateSig sk tr = C.validateSignature (C.sign sk tr) (C.Address $ C.derivePublicKey sk) tr

chooseAddressesJustWhenPossible :: NonEmptyList C.AddrId -> M.Map C.Color C.Coin  -> Bool
chooseAddressesJustWhenPossible (getNonEmpty -> adrlist) cmap =
    let adrCoinMap = C.coinsToMap $ map sel3 adrlist
        step color coin accum =
            let adrcn = C.getCoin $ M.findWithDefault 0 color adrCoinMap
                coin' = C.getCoin coin
            in (adrcn - coin') >= 0 && accum
        helper col cn = C.Coin col (C.getCoin cn)
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
        canonizeAddrIds = sort . filter ((/= 0) . C.getCoin . sel3)
    in case result of
           Nothing -> False
           Just resMap ->
               addrIdsEqual
                   (fst $ M.findWithDefault ([], undefined) col resMap)
                   addrIds
