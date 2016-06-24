{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | RSCoin.Core.Transaction specification

module Test.RSCoin.Core.TransactionSpec
       ( spec
       ) where

import           Data.Bifunctor             (first, second)
import           Data.List                  (genericLength)
import           Data.List                  (sort)
import qualified Data.Map.Strict            as M (Map, elems, findWithDefault,
                                                  foldrWithKey, fromListWith,
                                                  lookup, mapWithKey, null, (!))
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
        describe "validateSignature" $ do
            prop description_validateSignature validateSig
        describe "chooseAddresses" $ do
            prop description_chooseAddressesJustWhenPossible chooseAddressesJustWhenPossible
            prop description_chooseSmallerAddressesFirst chooseSmallerAddressesFirst
    where
      description_validateSumForValid =
        "returns true if total amount of grey coins in inputs is not less than " ++
        "amount of grey coins in outputs plus amount of coins spent to color coins"
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

{-
@alexrb: `chooseAddressesTest` is a good property that checks (as you wrote in description) that whenever it's possible to choose addresses somehow, addresses are actually chosen. Let's rename it to `chooseAddressesJustWhenPossible` (or similar name which reflects ​*which exactly property*​ of this function is checked). I think long names are acceptable (and even good) here.
Apart from that I want you to add one more property (after you debug `chooseOptimal` and fix it). Let's also check that `function takes addrids with smaller amount of money first` (as comment states).
For example:
* generate list of addrids with same color (coins are: `a1, a2, … a_n`), let's say `a_m = max(a1 … a_n) + 1` and color is `c`;
* also add `[a1 + a_m, a2 + a_m, … a_n + a_m]`;
* pass these `2 * n` addrids to `chooseAddresses`;
* coins map is `{c: sum(a1, … a_n)}`;
* check that result contains exactly `a1, a2, … a_n`;
* let's call it `chooseAddressesChoosesOptimal`.
What do you think about it?
-}

chooseSmallerAddressesFirst :: NonEmptyList C.AddrId -> Bool
chooseSmallerAddressesFirst (getNonEmpty -> adrList) =
    let col = C.getColor . sel3 . head $ adrList
        adrSameCol =
            map
                (\(h,i,C.Coin _ cn) ->
                      (h, i, C.Coin col cn))
                adrList
        coins = map sel3 adrSameCol
        maxCn = (maximum coins) + (C.Coin col 1)
        coins' = map (maxCn +) coins
        newAdrs =
            adrSameCol ++
            zipWith
                (\(h,i,_) cn ->
                      (h, i, cn))
                adrSameCol
                coins'
        cMap =
            M.fromListWith (+) $
            map
                (\c@(C.Coin cl _) ->
                      (cl, c))
                coins
        result = C.chooseAddresses newAdrs cMap
        addrIdsEqual :: [C.AddrId] -> [C.AddrId] -> Bool
        addrIdsEqual l1 l2 = canonizeAddrIds l1 == canonizeAddrIds l2
        canonizeAddrIds = sort . filter ((/= 0) . C.getCoin . sel3)
    in case result of
           Nothing -> False
           Just cMap' ->
               addrIdsEqual
                   (fst $ M.findWithDefault ([], undefined) col cMap')
                   adrSameCol
