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

roundTrip :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
roundTrip i = (fmap fromJSON . L.maybeResult . L.parse value . encode . toJSON) i == Just (Success i)

roundTripBool (v::Bool) = roundTrip v
roundTripDouble (v::Double) = roundTrip v
roundTripInteger (v::Integer) = roundTrip v

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
