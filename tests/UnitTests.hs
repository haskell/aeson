{-# LANGUAGE CPP, DeriveGeneric, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module UnitTests (ioTests, tests) where

import Control.Applicative (Const(..))
import Control.Monad (forM)
import Data.Aeson (decode, eitherDecode, encode, genericToJSON, genericToEncoding, object, FromJSON(..), withObject, (.=), (.:), (.:?), (.:!))
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson.Internal (JSONPathElement(..), formatError)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (ToJSON(..), Value, camelTo, camelTo2, defaultOptions, omitNothingFields)
import Data.Char (toUpper)
import Data.Hashable (hash)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq)
import Data.Tagged (Tagged(..))
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
  , testGroup "formatError" [
      testCase "example 1" $ formatErrorExample
    ]
  , testGroup ".:, .:?, .:!" $ fmap (testCase "-") dotColonMark
  , testGroup "To JSON representation" $ fmap (testCase "-") jsonEncoding
  , testGroup "From JSON representation" $ fmap (testCase "-") jsonDecoding
  , testGroup "JSONPath" $ fmap (testCase "-") jsonPath
  , testGroup "Hashable laws" $ fmap (testCase "-") hashableLaws
  , testGroup "Issue #351" $ fmap (testCase "-") issue351
  ]

roundTripCamel :: String -> Assertion
roundTripCamel name = assertEqual "" name (camelFrom '_' $ camelTo '_' name)

roundTripCamel2 :: String -> Assertion
roundTripCamel2 name = assertEqual "" name (camelFrom '_' $ camelTo2 '_' name)

camelFrom :: Char -> String -> String
camelFrom c s = let (p:ps) = split c s
                in concat $ p : map capitalize ps
  where
    split c' s' = map L.unpack $ L.split c' $ L.pack s'
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
                 wibbleString = replicate k 'a'
               , wibbleInt = 1
               }
    k | arch32bit = 4047
      | otherwise = 4030
    arch32bit     = (maxBound :: Int) == 2147483647

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

  -- Seconds in Time can be omitted
  let ts10 = "2015-01-03T12:13Z" :: LT.Text
  let ts11 = "2015-01-03 12:13Z" :: LT.Text
  let ts12 = "2015-01-01T12:30-02" :: LT.Text
  let (Just (t10 ::  UTCTime)) = parseWithAeson ts10
  let (Just (t11 ::  UTCTime)) = parseWithAeson ts11
  let (Just (t12 ::  UTCTime)) = parseWithAeson ts12
  assertEqual "utctime" (parseWithRead "%FT%H:%MZ" ts10) t10
  assertEqual "utctime" (parseWithRead "%F %H:%MZ" ts11) t11
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T14:30:00Z") t12

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
  verifyFailParse "2015-01-03 12:13.000Z" -- decimal at the end, but no seconds
  where
    verifyFailParse (s :: LT.Text) =
      let (dec :: Maybe UTCTime) = decode . LT.encodeUtf8 $ (LT.concat ["\"", s, "\""]) in
      assertEqual "verify failure" Nothing dec

-- Non identifier keys should be escaped & enclosed in brackets
formatErrorExample :: Assertion
formatErrorExample =
  let rhs = formatError [Index 0, Key "foo", Key "bar", Key "a.b.c", Key "", Key "'\\", Key "end"] "error msg"
      lhs = "Error in $[0].foo.bar['a.b.c']['']['\\'\\\\'].end: error msg"
  in assertEqual "formatError example" lhs rhs

------------------------------------------------------------------------------
-- Comparison (.:?) and (.:!)
------------------------------------------------------------------------------

newtype T1 = T1 (Maybe Int) deriving (Eq, Show)
newtype T2 = T2 (Maybe Int) deriving (Eq, Show)
newtype T3 = T3 (Maybe Int) deriving (Eq, Show)

instance FromJSON T1 where parseJSON = fmap T1 . withObject "T1" (.: "value")
instance FromJSON T2 where parseJSON = fmap T2 . withObject "T2" (.:? "value")
instance FromJSON T3 where parseJSON = fmap T3 . withObject "T3" (.:! "value")

dotColonMark :: [Assertion]
dotColonMark = [
    assertEqual ".:  not-present" Nothing               (decode ex1 :: Maybe T1)
  , assertEqual ".:  42"          (Just (T1 (Just 42))) (decode ex2 :: Maybe T1)
  , assertEqual ".:  null"        (Just (T1 Nothing))   (decode ex3 :: Maybe T1)

  , assertEqual ".:? not-present" (Just (T2 (Nothing))) (decode ex1 :: Maybe T2)
  , assertEqual ".:? 42"          (Just (T2 (Just 42))) (decode ex2 :: Maybe T2)
  , assertEqual ".:? null"        (Just (T2 Nothing))   (decode ex3 :: Maybe T2)

  , assertEqual ".:! not-present" (Just (T3 (Nothing))) (decode ex1 :: Maybe T3)
  , assertEqual ".:! 42"          (Just (T3 (Just 42))) (decode ex2 :: Maybe T3)
  , assertEqual ".:! null"        Nothing               (decode ex3 :: Maybe T3)
  ]
  where ex1 = "{}"
        ex2 = "{\"value\": 42 }"
        ex3 = "{\"value\": null }"

------------------------------------------------------------------------------
-- These tests assert that the JSON serialization doesn't change by accident.
-----------------------------------------------------------------------------

jsonEncoding :: [Assertion]
jsonEncoding = [
    assertEqual "Either Left" "{\"Left\":1}" $ encode (Left 1 :: Either Int Int)
  , assertEqual "Either Right" "{\"Right\":1}" $ encode (Right 1 :: Either Int Int)
  , assertEqual "Nothing"  "null" $ encode (Nothing :: Maybe Int)
  , assertEqual "Just"  "1" $ encode (Just 1 :: Maybe Int)
  , assertEqual "Just Nothing" "null" $ encode (Just Nothing :: Maybe (Maybe Int))
  , assertEqual "Proxy Int" "null" $ encode (Proxy :: Proxy Int)
  , assertEqual "Tagged Char Int" "1" $ encode (Tagged 1 :: Tagged Char Int)
  , assertEqual "Const Char Int" "\"c\"" $ encode (Const 'c' :: Const Char Int)
  , assertEqual "Tuple" "[1,2]" $ encode ((1, 2) :: (Int, Int))
  , assertEqual "NonEmpty" "[1,2,3]" $ encode (1 :| [2, 3] :: NonEmpty Int)
  , assertEqual "()" "[]" $ encode ()
  ]

jsonDecoding :: [Assertion]
jsonDecoding = [
    assertEqual "Nothing" (Nothing :: Maybe Int) (decode "null")
  , assertEqual "Just"    (Just 1 :: Maybe Int) (decode "1")
  , assertEqual "Just Nothing" (Just Nothing :: Maybe (Maybe Int)) (decode "null")
  , assertEqual "NonEmpty" (Just (1 :| [2, 3]) :: Maybe (NonEmpty Int)) (decode "[1,2,3]")
  , assertEqual "()" (Just ()) (decode "[]")
  ]

------------------------------------------------------------------------------
-- These tests check that JSONPath is tracked correctly
-----------------------------------------------------------------------------

jsonPath :: [Assertion]
jsonPath = [
    -- issue #356
    assertEqual "Either"
      (Left "Error in $[1].Left[1]: expected Bool, encountered Number")
      (eitherDecode "[1,{\"Left\":[2,3]}]"
         :: Either String (Int, Either (Int, Bool) ()))
    -- issue #358
  , assertEqual "Seq a"
      (Left "Error in $[2]: expected Int, encountered Boolean")
      (eitherDecode "[0,1,true]" :: Either String (Seq Int))
  ]

------------------------------------------------------------------------------
-- Check that the hashes of two equal Value are the same
------------------------------------------------------------------------------

hashableLaws :: [Assertion]
hashableLaws = [
    assertEqual "Hashable Object" (hash a) (hash b)
  ]
  where
  a = object ["223" .= False, "807882556" .= True]
  b = object ["807882556" .= True, "223" .= False]

------------------------------------------------------------------------------
-- Regressions
------------------------------------------------------------------------------

-- A regression test for: https://github.com/bos/aeson/issues/351
overlappingRegression :: FromJSON a => L.ByteString -> [a]
overlappingRegression bs = fromMaybe [] $ decode bs

issue351 :: [Assertion]
issue351 = [
    assertEqual "Int"  ([1, 2, 3] :: [Int])  $ overlappingRegression "[1, 2, 3]"
  , assertEqual "Char" (""        :: String) $ overlappingRegression "\"abc\""
  , assertEqual "Char" ("abc"     :: String) $ overlappingRegression "[\"a\", \"b\", \"c\"]"
  ]

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

-- A regression test for: https://github.com/bos/aeson/issues/293
data MyRecord = MyRecord {_field1 :: Maybe Int, _field2 :: Maybe Bool}
deriveJSON defaultOptions{omitNothingFields=True} ''MyRecord

data MyRecord2 = MyRecord2 {_field3 :: Maybe Int, _field4 :: Maybe Bool}
  deriving Generic

instance ToJSON   MyRecord2
instance FromJSON MyRecord2
