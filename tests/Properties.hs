{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Data.Aeson.Encode
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Attoparsec.Number
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Attoparsec.Lazy as L

encodeDouble num denom
    | isInfinite d || isNaN d = encode (Number (D d)) == "null"
    | otherwise               = encode (Number (D d)) == L.pack (show d)
  where d = num / denom
encodeInteger i = encode (Number (I i)) == L.pack (show i)

roundTrip :: (FromJSON a, ToJSON a) => (a -> a -> Bool) -> a -> Bool
roundTrip eq i =
    case fmap fromJSON . L.parse value . encode . toJSON $ i of
      L.Done _ (Success v) -> v `eq` i
      _                    -> False

roundTripBool (v::Bool) = roundTrip (==) v
roundTripDouble (v::Double) = roundTrip approxEq v
roundTripInteger (v::Integer) = roundTrip (==) v

approxEq :: Double -> Double -> Bool
approxEq a b = a == b ||
               d < maxAbsoluteError ||
                 d / max (abs b) (abs a) <= maxRelativeError
    where d = abs (a - b)
          maxAbsoluteError = 1e-15
          maxRelativeError = 1e-15

main = defaultMain tests

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
