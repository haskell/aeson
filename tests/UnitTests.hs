module UnitTests (ioTests, tests) where

import Control.Monad (forM)
import Data.Char (toUpper)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson.Types (Value, camelTo, toJSON)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertEqual)
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE

tests :: Test
tests = testGroup "unit" [
    testGroup "camelCase" [
      testCase "camelTo" $ roundTripCamel "aName"
    , testCase "camelTo" $ roundTripCamel "another"
    , testCase "camelTo" $ roundTripCamel "someOtherName"
    ]
  ]

roundTripCamel :: String -> Assertion
roundTripCamel name = assertEqual "" name (camelFrom '_' $ camelTo '_' name)


  where
    camelFrom c s = let (p:ps) = split c s
                    in concat $ p : map capitalize ps
    split c s = map L.unpack $ L.split c $ L.pack s
    capitalize t = toUpper (head t) : tail t

------------------------------------------------------------------------------
-- Comparison between bytestring and text encoders
------------------------------------------------------------------------------

ioTests :: IO [Test]
ioTests = do
  enc <- encoderComparisonTests
  return [enc]

encoderComparisonTests :: IO Test
encoderComparisonTests = do
  encoderTests <- forM testFiles $ \file0 -> do
      let file = "benchmarks/json-data/" ++ file0
      return $ testCase file $ do
          inp <- L.readFile file
          case eitherDecode inp of
            Left  err -> assertFailure $ "Decoding failure: " ++ err
            Right val -> assertEqual "" (encode val) (encodeViaText val)
  return $ testGroup "encoders" encoderTests
 where
  encodeViaText :: Value -> L.ByteString
  encodeViaText =
      TLE.encodeUtf8 . TLB.toLazyText . encodeToTextBuilder . toJSON

  testFiles =
    [ "example.json"
    , "integers.json"
    , "jp100.json"
    , "numbers.json"
    , "twitter10.json"
    , "twitter20.json"
    , "geometry.json"
    , "jp10.json"
    , "jp50.json"
    , "twitter1.json"
    , "twitter100.json"
    , "twitter50.json"
    ]
