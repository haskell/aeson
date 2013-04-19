{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

import Data.Aeson.Encode
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Attoparsec.Number
import Data.Data (Data)
import Data.Function (on)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.Aeson.Generic as G
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime(..))
import Data.Time (ZonedTime(..))
import Types (Foo(..), UFoo(..))
import Functions
import Instances ()

encodeDouble :: Double -> Double -> Bool
encodeDouble num denom
    | isInfinite d || isNaN d = encode (Number (D d)) == "null"
    | otherwise               = (read . L.unpack . encode . Number . D) d == d
  where d = num / denom

encodeInteger :: Integer -> Bool
encodeInteger i = encode (Number (I i)) == L.pack (show i)

roundTrip :: (FromJSON a, ToJSON a) => (a -> a -> Bool) -> a -> a -> Bool
roundTrip eq _ i =
    case fmap fromJSON . L.parse value . encode . toJSON $ i of
      L.Done _ (Success v) -> v `eq` i
      _                    -> False

roundTripEq :: (Eq a, FromJSON a, ToJSON a) => a -> a -> Bool
roundTripEq x y = roundTrip (==) x y

genericTo :: (Data a, ToJSON a) => a -> a -> Bool
genericTo _ v = G.toJSON v == toJSON v

genericFrom :: (Eq a, Data a, ToJSON a) => a -> a -> Bool
genericFrom _ v = G.fromJSON (toJSON v) == Success v

-- Compare equality to within a millisecond, allowing for rounding
-- error (ECMA 262 requires milliseconds to rounded to zero, not
-- rounded to nearest).
approxEqUTC :: UTCTime -> UTCTime -> Bool
approxEqUTC a b = ((==) `on` utctDay) a b &&
                  (approxEqWith 1 1 `on` ((* 1e3) . utctDayTime)) a b

approxEqNet :: DotNetTime -> DotNetTime -> Bool
approxEqNet (DotNetTime a) (DotNetTime b) = approxEqUTC a b

toFromJSON :: (Arbitrary a, Eq a, FromJSON a, ToJSON a) => a -> Bool
toFromJSON x = case fromJSON . toJSON $ x of
                Error _ -> False
                Success x' -> x == x'

genericToFromJSON :: (Arbitrary a, Eq a, Data a) => a -> Bool
genericToFromJSON x = case G.fromJSON . G.toJSON $ x of
                Error _ -> False
                Success x' -> x == x'

regress_gh72 :: [(String, Maybe String)] -> Bool
regress_gh72 ys = G.decode (G.encode m) == Just m
    where m = Map.fromList ys

modifyFailureProp :: String -> String -> Bool
modifyFailureProp orig added =
    result == Error (added ++ orig)
  where
    parser = const $ modifyFailure (added ++) $ fail orig
    result :: Result ()
    result = parse parser ()

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "regression" [
      testProperty "gh-72" regress_gh72
  ],
  testGroup "encode" [
      testProperty "encodeDouble" encodeDouble
    , testProperty "encodeInteger" encodeInteger
    ],
  testGroup "genericFrom" [
      testProperty "Bool" $ genericFrom True
    , testProperty "Double" $ genericFrom (1::Double)
    , testProperty "Int" $ genericFrom (1::Int)
    , testProperty "Foo" $ genericFrom (undefined::Foo)
    , testProperty "Maybe" $ genericFrom (Just 1 :: Maybe Int)
    ],
  testGroup "genericTo" [
      testProperty "Bool" $ genericTo True
    , testProperty "Double" $ genericTo (1::Double)
    , testProperty "Int" $ genericTo (1::Int)
    , testProperty "Foo" $ genericTo (undefined::Foo)
    , testProperty "Maybe" $ genericTo (Just 1 :: Maybe Int)
    ],
  testGroup "roundTrip" [
      testProperty "Bool" $ roundTripEq True
    , testProperty "Double" $ roundTrip approxEq (1::Double)
    , testProperty "Int" $ roundTripEq (1::Int)
    , testProperty "Integer" $ roundTripEq (1::Integer)
    , testProperty "String" $ roundTripEq (""::String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Foo" $ roundTripEq (undefined::Foo)
    , testProperty "DotNetTime" $ roundTrip approxEqNet undefined
    , testProperty "UTCTime" $ roundTrip approxEqUTC undefined
    , testProperty "ZonedTime" $ roundTripEq (undefined::ZonedTime)
    ],
  testGroup "toFromJSON" [
      testProperty "Integer" (toFromJSON :: Integer -> Bool)
    , testProperty "Double" (toFromJSON :: Double -> Bool)
    , testProperty "Maybe Integer" (toFromJSON :: Maybe Integer -> Bool)
    , testProperty "Either Integer Double" (toFromJSON :: Either Integer Double -> Bool)
    , testProperty "Either Integer Integer" (toFromJSON :: Either Integer Integer -> Bool)
    ],
  testGroup "genericToFromJSON" [
      testProperty "_UFoo" (genericToFromJSON :: UFoo -> Bool)
    ],
  testGroup "failure messages" [
      testProperty "modify failure" modifyFailureProp
    ]
  ]
