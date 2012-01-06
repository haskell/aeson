{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards,
    ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad
import Control.Applicative
import Data.Aeson.Encode
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Attoparsec.Number
import Data.Data (Typeable, Data)
import Data.Text (Text)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.Aeson.Generic as G
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Map as Map

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

approxEq :: Double -> Double -> Bool
approxEq a b = a == b ||
               d < maxAbsoluteError ||
                 d / max (abs b) (abs a) <= maxRelativeError
    where d = abs (a - b)
          maxAbsoluteError = 1e-15
          maxRelativeError = 1e-15

toFromJSON :: (Arbitrary a, Eq a, FromJSON a, ToJSON a) => a -> Bool
toFromJSON x = case fromJSON . toJSON $ x of
                Error _ -> False
                Success x' -> x == x'

genericToFromJSON :: (Arbitrary a, Eq a, Data a) => a -> Bool
genericToFromJSON x = case G.fromJSON . G.toJSON $ x of
                Error _ -> False
                Success x' -> x == x'


data Foo = Foo {
      fooInt :: Int
    , fooDouble :: Double
    , fooTuple :: (String, Text, Int)
    -- This definition causes an infinite loop in genericTo and genericFrom!
    -- , fooMap :: Map.Map String Foo
    , fooMap :: Map.Map String (Text,Int)
    } deriving (Show, Typeable, Data)

instance Eq Foo where
    a == b = fooInt a == fooInt b &&
             fooDouble a `approxEq` fooDouble b &&
             fooTuple a == fooTuple b

instance ToJSON Foo where
    toJSON Foo{..} = object [ "fooInt" .= fooInt
                            , "fooDouble" .= fooDouble
                            , "fooTuple" .= fooTuple
                            , "fooMap" .= fooMap
                            ]

instance FromJSON Foo where
    parseJSON (Object v) = Foo <$>
                           v .: "fooInt" <*>
                           v .: "fooDouble" <*>
                           v .: "fooTuple" <*>
                           v .: "fooMap"
    parseJSON _ = empty

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> arbitrary

instance Arbitrary Foo where
    arbitrary = liftM4 Foo arbitrary arbitrary arbitrary arbitrary

data UFoo = UFoo {
      _UFooInt :: Int
    , uFooInt :: Int
    } deriving (Show, Eq, Data, Typeable)

instance Arbitrary UFoo where
    arbitrary = UFoo <$> arbitrary <*> arbitrary
        where _ = uFooInt

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "encode" [
      testProperty "encodeDouble" encodeDouble
    , testProperty "encodeInteger" encodeInteger
    ],
  testGroup "genericFrom" [
      testProperty "Bool" $ genericFrom True
    , testProperty "Double" $ genericFrom (1::Double)
    , testProperty "Int" $ genericFrom (1::Int)
    , testProperty "Foo" $ genericFrom (undefined::Foo)
    ],
  testGroup "genericTo" [
      testProperty "Bool" $ genericTo True
    , testProperty "Double" $ genericTo (1::Double)
    , testProperty "Int" $ genericTo (1::Int)
    , testProperty "Foo" $ genericTo (undefined::Foo)
    ],
  testGroup "roundTrip" [
      testProperty "Bool" $ roundTripEq True
    , testProperty "Double" $ roundTrip approxEq (1::Double)
    , testProperty "Int" $ roundTripEq (1::Int)
    , testProperty "Integer" $ roundTripEq (1::Integer)
    , testProperty "String" $ roundTripEq (""::String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Foo" $ roundTripEq (undefined::Foo)
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
    ]
  ]
