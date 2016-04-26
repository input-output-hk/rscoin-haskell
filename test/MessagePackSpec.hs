{-# LANGUAGE ScopedTypeVariables #-}
-- | MessagePack specification. Testing problems with sending Maybe values.
-- There are more tests at
-- https://github.com/msgpack/msgpack-haskell

module MessagePackSpec
       ( spec
       ) where

import           Data.Maybe                 (fromJust)
import           Data.MessagePack           (MessagePack (..),
                                             pack, unpack)
-- import qualified Data.ByteString.Char8      as S
-- import qualified Data.ByteString.Lazy.Char8 as L
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            ((===))
-- import           Test.QuickCheck.Monadic    ()

spec :: Spec
spec =
    describe "MessagePack" $ do
        describe "Identity Properties" $ do
--            prop "int" $
--                \(a :: Int) -> a === mid a
--            prop "nil" $
--                \(a :: ()) -> a === mid a
--            prop "Double" $
--                \(a :: Double) -> a === mid a
--            prop "String" $
--                \(a :: String) -> a === mid a
--            prop "ByteString" $
--                \(a :: S.ByteString) -> a === mid a
--            prop "lazy ByteString" $
--                \(a :: L.ByteString) -> a === mid a
--            prop "[int]" $
--                \(a :: [Int]) -> a === mid a
--            prop "[string]" $
--                \(a :: [String]) -> a === mid a
--            prop "(int, int)" $
--                \(a :: (Int, Int)) -> a === mid a
--            prop "(int, int, int)" $
--                \(a :: (Int, Int, Int)) -> a === mid a
--            prop "(int, int, int, int)" $
--                \(a :: (Int, Int, Int, Int)) -> a === mid a
--            prop "(int, int, int, int, int)" $
--                \(a :: (Int, Int, Int, Int, Int)) -> a === mid a
--            prop "[(int, Double)]" $
--                \(a :: [(Int, Double)]) -> a === mid a
--            prop "[(string, string)]" $
--                \(a :: [(String, String)]) -> a === mid a
--            prop "Assoc [(string, int)]" $
--                \(a :: Assoc [(String, Int)]) -> a === mid a

--          -- TODO: rest of commented properties are already tested in
--          -- msgpack tests but they didn't test Maybe so here it is.
--          -- With all props tests are running too long
            prop "maybe int" $
                \(a :: Maybe Int) -> a === mid a

--            prop "maybe nil" $
--                \(a :: Maybe ()) -> a === mid a
--            prop "maybe Double" $
--                \(a :: Maybe Double) -> a === mid a
--            prop "maybe String" $
--                \(a :: Maybe String) -> a === mid a
--            prop "maybe ByteString" $
--                \(a :: Maybe S.ByteString) -> a === mid a
--            prop "maybe lazy ByteString" $
--                \(a :: Maybe L.ByteString) -> a === mid a
--            prop "maybe [int]" $
--                \(a :: Maybe [Int]) -> a === mid a
--            prop "maybe [string]" $
--                \(a :: Maybe [String]) -> a === mid a
--            prop "maybe (int, int)" $
--                \(a :: Maybe (Int, Int)) -> a === mid a
--            prop "maybe (int, int, int)" $
--                \(a :: Maybe (Int, Int, Int)) -> a === mid a
--            prop "maybe (int, int, int, int)" $
--                \(a :: Maybe (Int, Int, Int, Int)) -> a === mid a
--            prop "maybe (int, int, int, int, int)" $
--                \(a :: Maybe (Int, Int, Int, Int, Int)) -> a === mid a
--            prop "maybe [(int, Double)]" $
--                \(a :: Maybe [(Int, Double)]) -> a === mid a
--            prop "maybe [(string, string)]" $
--                \(a :: Maybe [(String, String)]) -> a === mid a
--            prop "maybe (Assoc [(string, int)])" $
--                \(a :: Maybe (Assoc [(String, Int)])) -> a === mid a

--instance Arbitrary a => Arbitrary (Assoc a) where
--  arbitrary = Assoc <$> arbitrary
--
--instance Arbitrary S.ByteString where
--  arbitrary = S.pack <$> arbitrary
--
--instance Arbitrary L.ByteString where
--  arbitrary = L.pack <$> arbitrary

mid :: MessagePack a => a -> a
mid = fromJust . unpack . pack
