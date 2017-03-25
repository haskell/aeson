{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DataKinds #-}
#endif

------------------------------------------------------------------------------
-- These tests assert that the JSON serialization doesn't change by accident.
-----------------------------------------------------------------------------

module SerializationFormatSpec
  (
    tests
  ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (Const(..))
import Data.Aeson (FromJSON(..), decode, encode, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options(..), SumEncoding(..), ToJSON(..), defaultOptions)
import Data.Fixed (Pico)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific)
import Data.Tagged (Tagged(..))
import Data.Time (fromGregorian)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Instances ()
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure, assertEqual)
import Types (Approx(..), Compose3, Compose3', I)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as Vector

tests :: [Test]
tests =
  [
    testGroup "To JSON representation" $ fmap assertJsonEncodingExample jsonEncodingExamples
  , testGroup "From JSON representation" $ fmap assertJsonExample jsonDecodingExamples
  , testGroup "To/From JSON representation" $ fmap assertJsonExample jsonExamples

  ]

jsonExamples :: [Example]
jsonExamples =
  [
    Example "Either Left" "{\"Left\":1}"  (Left 1 :: Either Int Int)
  , Example "Either Right" "{\"Right\":1}"  (Right 1 :: Either Int Int)
  , Example "Nothing"  "null"  (Nothing :: Maybe Int)
  , Example "Just"  "1"  (Just 1 :: Maybe Int)
  , Example "Proxy Int" "null"  (Proxy :: Proxy Int)
  , Example "Tagged Char Int" "1"  (Tagged 1 :: Tagged Char Int)
#if __GLASGOW_HASKELL__ >= 708
    -- Test Tagged instance is polykinded
  , Example "Tagged 123 Int" "1"  (Tagged 1 :: Tagged 123 Int)
#endif
  , Example "Const Char Int" "\"c\""  (Const 'c' :: Const Char Int)
  , Example "Tuple" "[1,2]"  ((1, 2) :: (Int, Int))
  , Example "NonEmpty" "[1,2,3]"  (1 :| [2, 3] :: NonEmpty Int)
  , Example "Seq" "[1,2,3]"  (Seq.fromList [1, 2, 3] ::  Seq.Seq Int)
  , Example "DList" "[1,2,3]"  (DList.fromList [1, 2, 3] :: DList.DList Int)
  , Example "()" "[]"  ()

  , Example "HashMap Int Int"          "{\"0\":1,\"2\":3}"  (HM.fromList [(0,1),(2,3)] :: HM.HashMap Int Int)
  , Example "Map Int Int"              "{\"0\":1,\"2\":3}"  (M.fromList [(0,1),(2,3)] :: M.Map Int Int)
  , Example "Map (Tagged Int Int) Int" "{\"0\":1,\"2\":3}"  (M.fromList [(Tagged 0,1),(Tagged 2,3)] :: M.Map (Tagged Int Int) Int)
  , Example "Map [Int] Int"            "[[[0],1],[[2],3]]"  (M.fromList [([0],1),([2],3)] :: M.Map [Int] Int)
  , Example "Map [Char] Int"           "{\"ab\":1,\"cd\":3}"  (M.fromList [("ab",1),("cd",3)] :: M.Map String Int)
  , Example "Map [I Char] Int"         "{\"ab\":1,\"cd\":3}"  (M.fromList [(map pure "ab",1),(map pure "cd",3)] :: M.Map [I Char] Int)

  , Example "nan :: Double" "null"  (Approx $ 0/0 :: Approx Double)

  , Example "Ordering LT" "\"LT\"" LT
  , Example "Ordering EQ" "\"EQ\"" EQ
  , Example "Ordering GT" "\"GT\"" GT

  , Example "Float" "3.14" (3.14 :: Float)
  , Example "Pico" "3.14" (3.14 :: Pico)
  , Example "Scientific" "3.14" (3.14 :: Scientific)

  , Example "UUID" "\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\"" $ UUID.fromWords
      0xc2cc10e1 0x57d64b6f 0x989938d9 0x72112d8c

  , Example "Set Int" "[1,2,3]" (Set.fromList [3, 2, 1] :: Set.Set Int)
  , Example "IntSet"  "[1,2,3]" (IntSet.fromList [3, 2, 1])
  , Example "IntMap" "[[1,2],[3,4]]" (IntMap.fromList [(3,4), (1,2)] :: IntMap.IntMap Int)
  , Example "Vector" "[1,2,3]" (Vector.fromList [1, 2, 3] :: Vector.Vector Int)
  , Example "HashSet Int" "[1,2,3]" (HashSet.fromList [3, 2, 1] :: HashSet.HashSet Int)
  , Example "Tree Int" "[1,[[2,[[3,[]],[4,[]]]],[5,[]]]]" (let n = Tree.Node in n 1 [n 2 [n 3 [], n 4 []], n 5 []] :: Tree.Tree Int)

  -- Three separate cases, as ordering in HashMap is not defined
  , Example "HashMap Float Int, NaN" "{\"NaN\":1}"  (Approx $ HM.singleton (0/0) 1 :: Approx (HM.HashMap Float Int))
  , Example "HashMap Float Int, Infinity" "{\"Infinity\":1}"  (HM.singleton (1/0) 1 :: HM.HashMap Float Int)
  , Example "HashMap Float Int, +Infinity" "{\"-Infinity\":1}"  (HM.singleton (negate 1/0) 1 :: HM.HashMap Float Int)

  -- Functors
  , Example "Identity Int" "1"  (pure 1 :: Identity Int)

  , Example "Identity Char" "\"x\""      (pure 'x' :: Identity Char)
  , Example "Identity String" "\"foo\""  (pure "foo" :: Identity String)
  , Example "[Identity Char]" "\"xy\""   ([pure 'x', pure 'y'] :: [Identity Char])

  , Example "Maybe Char" "\"x\""              (pure 'x' :: Maybe Char)
  , Example "Maybe String" "\"foo\""          (pure "foo" :: Maybe String)
  , Example "Maybe [Identity Char]" "\"xy\""  (pure [pure 'x', pure 'y'] :: Maybe [Identity Char])

  , Example "Day; year >= 1000" "\"1999-10-12\""        (fromGregorian 1999    10 12)
  , Example "Day; year > 0 && < 1000" "\"0500-03-04\""  (fromGregorian 500     3  4)
  , Example "Day; year == 0" "\"0000-02-20\""           (fromGregorian 0       2  20)
  , Example "Day; year < 0" "\"-0234-01-01\""           (fromGregorian (-234)  1  1)
  , Example "Day; year < -1000" "\"-1234-01-01\""       (fromGregorian (-1234) 1  1)

  , Example "Product I Maybe Int" "[1,2]"         (Pair (pure 1) (pure 2) :: Product I Maybe Int)
  , Example "Product I Maybe Int" "[1,null]"      (Pair (pure 1) Nothing :: Product I Maybe Int)
  , Example "Product I [] Char" "[\"a\",\"foo\"]" (Pair (pure 'a') "foo" :: Product I [] Char)

  , Example "Sum I [] Int: InL"  "{\"InL\":1}"       (InL (pure 1) :: Sum I [] Int)
  , Example "Sum I [] Int: InR"  "{\"InR\":[1,2]}"   (InR [1, 2] :: Sum I [] Int)
  , Example "Sum I [] Char: InR" "{\"InR\":\"foo\"}" (InR "foo" :: Sum I [] Char)

  , Example "Compose I  I  Int" "1"      (pure 1 :: Compose I I   Int)
  , Example "Compose I  [] Int" "[1]"    (pure 1 :: Compose I []  Int)
  , Example "Compose [] I  Int" "[1]"    (pure 1 :: Compose [] I  Int)
  , Example "Compose [] [] Int" "[[1]]"  (pure 1 :: Compose [] [] Int)

  , Example "Compose I  I  Char" "\"x\""    (pure 'x' :: Compose I  I  Char)
  , Example "Compose I  [] Char" "\"x\""    (pure 'x' :: Compose I  [] Char)
  , Example "Compose [] I  Char" "\"x\""    (pure 'x' :: Compose [] I  Char)
  , Example "Compose [] [] Char" "[\"x\"]"  (pure 'x' :: Compose [] [] Char)

  , Example "Compose3 I  I  I  Char" "\"x\""      (pure 'x' :: Compose3 I  I  I  Char)
  , Example "Compose3 I  I  [] Char" "\"x\""      (pure 'x' :: Compose3 I  I  [] Char)
  , Example "Compose3 I  [] I  Char" "\"x\""      (pure 'x' :: Compose3 I  [] I  Char)
  , Example "Compose3 I  [] [] Char" "[\"x\"]"    (pure 'x' :: Compose3 I  [] [] Char)
  , Example "Compose3 [] I  I  Char" "\"x\""      (pure 'x' :: Compose3 [] I  I  Char)
  , Example "Compose3 [] I  [] Char" "[\"x\"]"    (pure 'x' :: Compose3 [] I  [] Char)
  , Example "Compose3 [] [] I  Char" "[\"x\"]"    (pure 'x' :: Compose3 [] [] I  Char)
  , Example "Compose3 [] [] [] Char" "[[\"x\"]]"  (pure 'x' :: Compose3 [] [] [] Char)

  , Example "Compose3' I  I  I  Char" "\"x\""      (pure 'x' :: Compose3' I  I  I  Char)
  , Example "Compose3' I  I  [] Char" "\"x\""      (pure 'x' :: Compose3' I  I  [] Char)
  , Example "Compose3' I  [] I  Char" "\"x\""      (pure 'x' :: Compose3' I  [] I  Char)
  , Example "Compose3' I  [] [] Char" "[\"x\"]"    (pure 'x' :: Compose3' I  [] [] Char)
  , Example "Compose3' [] I  I  Char" "\"x\""      (pure 'x' :: Compose3' [] I  I  Char)
  , Example "Compose3' [] I  [] Char" "[\"x\"]"    (pure 'x' :: Compose3' [] I  [] Char)
  , Example "Compose3' [] [] I  Char" "[\"x\"]"    (pure 'x' :: Compose3' [] [] I  Char)
  , Example "Compose3' [] [] [] Char" "[[\"x\"]]"  (pure 'x' :: Compose3' [] [] [] Char)

  , Example "MyEither Int String: Left"  "42"      (MyLeft 42     :: MyEither Int String)
  , Example "MyEither Int String: Right" "\"foo\"" (MyRight "foo" :: MyEither Int String)

  -- newtypes from Monoid/Semigroup
  , Example "Monoid.Dual Int" "2" (pure 2 :: Monoid.Dual Int)
  , Example "Monoid.First Int" "2" (pure 2 :: Monoid.First Int)
  , Example "Monoid.Last Int" "2" (pure 2 :: Monoid.Last Int)
  , Example "Semigroup.Min Int" "2" (pure 2 :: Semigroup.Min Int)
  , Example "Semigroup.Max Int" "2" (pure 2 :: Semigroup.Max Int)
  , Example "Semigroup.First Int" "2" (pure 2 :: Semigroup.First Int)
  , Example "Semigroup.Last Int" "2" (pure 2 :: Semigroup.Last Int)
  , Example "Semigroup.WrappedMonoid Int" "2" (Semigroup.WrapMonoid 2 :: Semigroup.WrappedMonoid Int)
  , Example "Semigroup.Option Just" "2" (pure 2 :: Semigroup.Option Int)
  , Example "Semigroup.Option Nothing" "null" (Semigroup.Option (Nothing :: Maybe Bool))
  ]

jsonEncodingExamples :: [Example]
jsonEncodingExamples =
  [
  -- Maybe serialising is lossy
  -- https://github.com/bos/aeson/issues/376
    Example "Just Nothing" "null" (Just Nothing :: Maybe (Maybe Int))
  -- infinities cannot be recovered, null is decoded as NaN
  , Example "inf :: Double" "null" (Approx $ 1/0 :: Approx Double)
  ]

jsonDecodingExamples :: [Example]
jsonDecodingExamples = [
  -- Maybe serialising is lossy
  -- https://github.com/bos/aeson/issues/376
    MaybeExample "Nothing"      "null" (Just Nothing :: Maybe (Maybe Int))
  , MaybeExample "Just"         "1"    (Just $ Just 1 :: Maybe (Maybe Int))
  , MaybeExample "Just Nothing" "null" (Just Nothing :: Maybe (Maybe (Maybe Int)))
  -- Integral values are truncated, and overflowed
  -- https://github.com/bos/aeson/issues/317
  , MaybeExample "Word8 3"    "3"    (Just 3 :: Maybe Word8)
  , MaybeExample "Word8 3.00" "3.00" (Just 3 :: Maybe Word8)
  , MaybeExample "Word8 3.14" "3.14" (Nothing :: Maybe Word8)
  , MaybeExample "Word8 -1"   "-1"   (Nothing :: Maybe Word8)
  , MaybeExample "Word8 300"  "300"  (Nothing :: Maybe Word8)
  -- Negative zero year, encoding never produces such:
  , MaybeExample "Day -0000-02-03" "\"-0000-02-03\"" (Just (fromGregorian 0 2 3))
  ]

data Example where
    Example
        :: (Eq a, Show a, ToJSON a, FromJSON a)
        => String -> L.ByteString -> a -> Example
    MaybeExample
        :: (Eq a, Show a, FromJSON a)
        => String -> L.ByteString -> Maybe a -> Example

data MyEither a b = MyLeft a | MyRight b
  deriving (Generic, Show, Eq)

instance (ToJSON a, ToJSON b) => ToJSON (MyEither a b) where
    toJSON = genericToJSON defaultOptions { sumEncoding = UntaggedValue }
    toEncoding = genericToEncoding defaultOptions { sumEncoding = UntaggedValue }

instance (FromJSON a, FromJSON b) => FromJSON (MyEither a b) where
    parseJSON = genericParseJSON defaultOptions { sumEncoding = UntaggedValue }

assertJsonExample :: Example -> Test
assertJsonExample (Example name bs val) = testCase name $ do
    assertEqual "encode"           bs         (encode val)
    assertEqual "encode/via value" bs         (encode $ toJSON val)
    assertEqual "decode"           (Just val) (decode bs)
assertJsonExample (MaybeExample name bs mval) = testCase name $
    assertEqual "decode" mval (decode bs)

assertJsonEncodingExample :: Example -> Test
assertJsonEncodingExample (Example name bs val) = testCase name $ do
    assertEqual "encode"           bs (encode val)
    assertEqual "encode/via value" bs (encode $ toJSON val)
assertJsonEncodingExample (MaybeExample name _ _) = testCase name $
    assertFailure "cannot encode MaybeExample"
