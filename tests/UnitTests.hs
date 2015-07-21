{-# LANGUAGE CPP, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module UnitTests (ioTests, tests) where

import Control.Monad (forM)
import Data.Aeson (decode, eitherDecode, encode, genericToJSON, genericToEncoding)
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson.Types (ToJSON(..), Value, camelTo, camelTo2, defaultOptions)
import Data.Char (toUpper)
import Data.Time (UTCTime)
import Data.Time.Format (parseTime)
import GHC.Generics (Generic)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertEqual)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

tests :: Test
tests = testGroup "unit" [
    testGroup "camelCase" [
      testCase "camelTo" $ roundTripCamel "aName"
    , testCase "camelTo" $ roundTripCamel "another"
    , testCase "camelTo" $ roundTripCamel "someOtherName"
    , testCase "camelTo" $
        assertEqual "" "camel_apicase" (camelTo '_' "CamelAPICase")
    , testCase "camelTo2" $ roundTripCamel2 "aName"
    , testCase "camelTo2" $ roundTripCamel2 "another"
    , testCase "camelTo2" $ roundTripCamel2 "someOtherName"
    , testCase "camelTo2" $
        assertEqual "" "camel_api_case" (camelTo2 '_' "CamelAPICase")
    ]
  , testGroup "encoding" [
      testCase "goodProducer" $ goodProducer
    ]
  , testGroup "utctime" [
      testCase "good" $ utcTimeGood
    , testCase "bad"  $ utcTimeBad
    ]
  ]

roundTripCamel :: String -> Assertion
roundTripCamel name = assertEqual "" name (camelFrom '_' $ camelTo '_' name)

roundTripCamel2 :: String -> Assertion
roundTripCamel2 name = assertEqual "" name (camelFrom '_' $ camelTo2 '_' name)

camelFrom :: Char -> String -> String
camelFrom c s = let (p:ps) = split c s
                in concat $ p : map capitalize ps
  where
    split c s = map L.unpack $ L.split c $ L.pack s
    capitalize t = toUpper (head t) : tail t

data Wibble = Wibble {
    wibbleString :: String
  , wibbleInt :: Int
  } deriving (Generic, Show)

instance ToJSON Wibble where
    toJSON     = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

-- Test that if we put a bomb in a data structure, but only demand
-- part of it via lazy encoding, we do not unexpectedly fail.
goodProducer :: Assertion
goodProducer = assertEqual "partial encoding should not explode on undefined"
                           '{' (L.head (encode wibble))
  where
    wibble = Wibble {
                 wibbleString = replicate 4030 'a'
               , wibbleInt = undefined
               }

-- Test decoding various UTC time formats
--
-- Note: the incomplete pattern matches for UTCTimes are completely
-- intentional.  The test expects these parses to succeed.  If the
-- pattern matches fails, there's a bug in either the test or in aeson
-- and needs to be investigated.
utcTimeGood :: Assertion
utcTimeGood = do
  let ts1 = "2015-01-01T12:13:00.00Z" :: LT.Text
  let ts2 = "2015-01-01T12:13:00Z" :: LT.Text
  -- 'T' between date and time is not required, can be space
  let ts3 = "2015-01-03 12:13:00.00Z" :: LT.Text
  let ts4 = "2015-01-03 12:13:00.125Z" :: LT.Text
  let (Just (t1 ::  UTCTime)) = parseWithAeson ts1
  let (Just (t2 ::  UTCTime)) = parseWithAeson ts2
  let (Just (t3 ::  UTCTime)) = parseWithAeson ts3
  let (Just (t4 ::  UTCTime)) = parseWithAeson ts4
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" ts1) t1
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" ts2) t2
  assertEqual "utctime" (parseWithRead "%F %T%QZ" ts3) t3
  assertEqual "utctime" (parseWithRead "%F %T%QZ" ts4) t4
  -- Time zones.  Both +HHMM and +HH:MM are allowed for timezone
  -- offset, and MM may be omitted.
  let ts5 = "2015-01-01T12:30:00.00+00" :: LT.Text
  let ts6 = "2015-01-01T12:30:00.00+01:15" :: LT.Text
  let ts7 = "2015-01-01T12:30:00.00-02" :: LT.Text
  let ts8 = "2015-01-01T22:00:00.00-03" :: LT.Text
  let ts9 = "2015-01-01T22:00:00.00-04:30" :: LT.Text
  let (Just (t5 ::  UTCTime)) = parseWithAeson ts5
  let (Just (t6 ::  UTCTime)) = parseWithAeson ts6
  let (Just (t7 ::  UTCTime)) = parseWithAeson ts7
  let (Just (t8 ::  UTCTime)) = parseWithAeson ts8
  let (Just (t9 ::  UTCTime)) = parseWithAeson ts9
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T12:30:00.00Z") t5
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T11:15:00.00Z") t6
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T14:30:00Z") t7
  -- ts8 wraps around to the next day in UTC
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-02T01:00:00Z") t8
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-02T02:30:00Z") t9
  where
    parseWithRead :: String -> LT.Text -> UTCTime
    parseWithRead f s =
      case parseTime defaultTimeLocale f . LT.unpack $ s of
        Nothing -> error "parseTime input malformed"
        Just t  -> t
    parseWithAeson :: LT.Text -> Maybe UTCTime
    parseWithAeson s = decode . LT.encodeUtf8 $ (LT.concat ["\"", s, "\""])

-- Test that a few non-timezone qualified timestamp formats get
-- rejected if decoding to UTCTime.
utcTimeBad :: Assertion
utcTimeBad = do
  verifyFailParse "2000-01-01T12:13:00" -- missing Zulu time not allowed (some TZ required)
  verifyFailParse "2000-01-01 12:13:00" -- missing Zulu time not allowed (some TZ required)
  verifyFailParse "2000-01-01"          -- date only not OK
  verifyFailParse "2000-01-01Z"         -- date only not OK
  verifyFailParse "2015-01-01T12:30:00.00+00Z" -- no Zulu if offset given
  verifyFailParse "2015-01-01T12:30:00.00+00:00Z" -- no Zulu if offset given
  verifyFailParse "2015-01-03 12:13:00.Z" -- decimal at the end but no digits
  where
    verifyFailParse (s :: LT.Text) =
      let (dec :: Maybe UTCTime) = decode . LT.encodeUtf8 $ (LT.concat ["\"", s, "\""]) in
      assertEqual "verify failure" Nothing dec


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
