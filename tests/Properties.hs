{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Applicative
import Data.Aeson.Encode
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Attoparsec.Number
import Data.Text (Text)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T

encodeDouble :: Double -> Double -> Bool
encodeDouble num denom
    | isInfinite d || isNaN d = encode (Number (D d)) == "null"
    | otherwise               = (read . L.unpack . encode . Number . D) d == d
  where d = num / denom

encodeInteger :: Integer -> Bool
encodeInteger i = encode (Number (I i)) == L.pack (show i)

roundTrip :: (FromJSON a, ToJSON a) => (a -> a -> Bool) -> a -> Bool
roundTrip eq i =
    case fmap fromJSON . L.parse value . encode . toJSON $ i of
      L.Done _ (Success v) -> v `eq` i
      _                    -> False

roundTripBool :: Bool -> Bool
roundTripBool = roundTrip (==)
roundTripDouble :: Double -> Bool
roundTripDouble = roundTrip approxEq
roundTripInteger :: Integer -> Bool
roundTripInteger = roundTrip (==)
roundTripFoo :: Foo -> Bool
roundTripFoo = roundTrip (==)

approxEq :: Double -> Double -> Bool
approxEq a b = a == b ||
               d < maxAbsoluteError ||
                 d / max (abs b) (abs a) <= maxRelativeError
    where d = abs (a - b)
          maxAbsoluteError = 1e-15
          maxRelativeError = 1e-15

data Foo = Foo {
      fooInt :: Int
    , fooDouble :: Double
    , fooTuple :: (String, Text)
    } deriving (Show)

instance Eq Foo where
    a == b = fooInt a == fooInt b &&
             fooDouble a `approxEq` fooDouble b &&
             fooTuple a == fooTuple b

instance ToJSON Foo where
    toJSON Foo{..} = object [ "int" .= fooInt
                            , "double" .= fooDouble
                            , "tuple" .= fooTuple
                            ]

instance FromJSON Foo where
    parseJSON (Object v) = Foo <$>
                           v .: "int" <*>
                           v .: "double" <*>
                           v .: "tuple"
    parseJSON _ = empty

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary Foo where
    arbitrary = liftA3 Foo arbitrary arbitrary arbitrary

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "encode" [
      testProperty "encodeDouble" encodeDouble
    , testProperty "encodeInteger" encodeInteger
    ],
  testGroup "roundTrip" [
      testProperty "roundTripBool" roundTripBool
    , testProperty "roundTripDouble" roundTripDouble
    , testProperty "roundTripInteger" roundTripInteger
    , testProperty "roundTripFoo" roundTripFoo
    ]
  ]
