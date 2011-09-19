{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Data.Aeson.Encode
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Attoparsec.Number
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Attoparsec.Lazy as L

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

approxEq :: Double -> Double -> Bool
approxEq a b = a == b ||
               d < maxAbsoluteError ||
                 d / max (abs b) (abs a) <= maxRelativeError
    where d = abs (a - b)
          maxAbsoluteError = 1e-15
          maxRelativeError = 1e-15

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
    ]
  ]
