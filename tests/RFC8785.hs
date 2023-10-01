{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module RFC8785 (rfc8785Tests) where

import Data.Text (Text)
import Data.Word (Word64)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck (testProperty, (===))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Scientific as Sci

import Data.Aeson
import Data.Aeson.Decoding.ByteString.Lazy
import Data.Aeson.Decoding.Tokens
import Data.Aeson.QQ.Simple
import Data.Aeson.RFC8785

import CastFloat
import DoubleToScientific

rfc8785Tests :: TestTree
rfc8785Tests = testGroup "RFC8785"
    [ testCase "example" $ do
        encodeCanonical exampleValue @?= exampleBS

    , testCase "key-sort" $ do
        recordTextValues (encodeCanonical sortingValue) @?= sortingStrings

    , numberCases

    , testProperty "roundtrip" $ \v ->
        decode (encodeCanonical v) === Just (v :: Value)

    , castFloatTests
    , doubleToScientificTests
    ]

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

exampleValue :: Value
exampleValue = mapNumbers (doubleToScientific . Sci.toRealFloat) [aesonQQ| {
    "numbers": [333333333.33333329, 1E30, 4.50,
                2e-3, 0.000000000000000000000000001],
    "string": "\u20ac$\u000F\u000aA'\u0042\u0022\u005c\\\"\/",
    "literals": [null, true, false]
  } |]

mapNumbers :: (Sci.Scientific -> Sci.Scientific) -> Value -> Value
mapNumbers _ v@Null        = v
mapNumbers _ v@(String _)  = v
mapNumbers _ v@(Bool _)    = v
mapNumbers f   (Number x)  = Number (f x)
mapNumbers f   (Object xs) = Object (fmap (mapNumbers f) xs)
mapNumbers f   (Array xs)  = Array (fmap (mapNumbers f) xs)


exampleBS :: LBS.ByteString
exampleBS = LBS.pack
    [ 0x7b, 0x22, 0x6c, 0x69, 0x74, 0x65, 0x72, 0x61, 0x6c, 0x73, 0x22, 0x3a, 0x5b, 0x6e, 0x75, 0x6c, 0x6c, 0x2c, 0x74, 0x72
    , 0x75, 0x65, 0x2c, 0x66, 0x61, 0x6c, 0x73, 0x65, 0x5d, 0x2c, 0x22, 0x6e, 0x75, 0x6d, 0x62, 0x65, 0x72, 0x73, 0x22, 0x3a
    , 0x5b, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x2e, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x2c, 0x31
    , 0x65, 0x2b, 0x33, 0x30, 0x2c, 0x34, 0x2e, 0x35, 0x2c, 0x30, 0x2e, 0x30, 0x30, 0x32, 0x2c, 0x31, 0x65, 0x2d, 0x32, 0x37
    , 0x5d, 0x2c, 0x22, 0x73, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x22, 0x3a, 0x22, 0xe2, 0x82, 0xac, 0x24, 0x5c, 0x75, 0x30, 0x30
    , 0x30, 0x66, 0x5c, 0x6e, 0x41, 0x27, 0x42, 0x5c, 0x22, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x22, 0x2f, 0x22, 0x7d
    ]

-------------------------------------------------------------------------------
-- Key sorting
-------------------------------------------------------------------------------

sortingValue :: Value
sortingValue = [aesonQQ| {
    "\u20ac": "Euro Sign",
    "\r": "Carriage Return",
    "\ufb33": "Hebrew Letter Dalet With Dagesh",
    "1": "One",
    "\ud83d\ude00": "Emoji: Grinning Face",
    "\u0080": "Control",
    "\u00f6": "Latin Small Letter O With Diaeresis"
  } |]

sortingStrings :: [Text]
sortingStrings =
    [ "Carriage Return"
    , "One"
    , "Control"
    , "Latin Small Letter O With Diaeresis"
    , "Euro Sign"
    , "Emoji: Grinning Face"
    , "Hebrew Letter Dalet With Dagesh"
    ]

recordTextValues :: LBS.ByteString -> [Text]
recordTextValues lbs = case lbsToTokens lbs of
    TkRecordOpen r -> go r
    _              -> []
  where
    go (TkPair _ (TkText s ts)) = s : go ts
    go _                        = []

-------------------------------------------------------------------------------
-- Numbers
-------------------------------------------------------------------------------

-- Appendix B. Number Serialization Samples
numbers :: [(Word64, LBS.ByteString)]
numbers =
    [ (0x0000000000000000, "0") -- Zero
    , (0x8000000000000000, "0") -- Minus zero
    , (0x0000000000000001, "5e-324") -- Min pos number
    , (0x8000000000000001, "-5e-324") -- Min neg number
    , (0x7fefffffffffffff, "1.7976931348623157e+308") -- Max pos number
    , (0xffefffffffffffff, "-1.7976931348623157e+308") -- Max neg number
    , (0x4340000000000000, "9007199254740992") -- Max pos int (1)
    , (0xc340000000000000, "-9007199254740992") -- Max neg int (1)
    , (0x4430000000000000, "295147905179352830000") -- ~2**68 (2)
    , (0x44b52d02c7e14af5, "9.999999999999997e+22")
    , (0x44b52d02c7e14af6, "1e+23")
    , (0x44b52d02c7e14af7, "1.0000000000000001e+23")
    , (0x444b1ae4d6e2ef4e, "999999999999999700000")
    , (0x444b1ae4d6e2ef4f, "999999999999999900000")
    , (0x444b1ae4d6e2ef50, "1e+21")
    , (0x3eb0c6f7a0b5ed8c, "9.999999999999997e-7")
    , (0x3eb0c6f7a0b5ed8d, "0.000001")
    , (0x41b3de4355555553, "333333333.3333332")
    , (0x41b3de4355555554, "333333333.33333325")
    , (0x41b3de4355555555, "333333333.3333333")
    , (0x41b3de4355555556, "333333333.3333334")
    , (0x41b3de4355555557, "333333333.33333343")
    , (0xbecbf647612f3696, "-0.0000033333333333333333")
    , (0x43143ff3c1cb0959, "1424953923781206.2") -- Round to even (4)
    ]

numberCases :: TestTree
numberCases = testGroup "numbers"
    [ testCase (LBS8.unpack lbs) $ do
        encodeCanonical (doubleToScientific d) @?= lbs
    | (w64, lbs) <- numbers
    , let d = castWord64ToDouble w64 :: Double
    ]
