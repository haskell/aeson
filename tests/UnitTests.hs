{-# LANGUAGE CPP, GADTs, DeriveGeneric, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, GeneralizedNewtypeDeriving #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DataKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module UnitTests (ioTests, tests) where

import Prelude ()
import Prelude.Compat

import Types (I, Compose3, Compose3', Approx(..))
import Instances ()

import Control.Applicative (Const(..))
import Control.Monad (forM)
import Data.Aeson (FromJSONKeyFunction(..), FromJSONKey(..), decode, eitherDecode, encode, genericToJSON, genericToEncoding, object, FromJSON(..), withObject, (.=), (.:), (.:?), (.:!))
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Aeson.Internal (JSONPathElement(..), formatError)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (ToJSON(..), Value, camelTo, camelTo2, defaultOptions, omitNothingFields)
import Data.Char (toUpper)
import Data.Hashable (hash)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Format (parseTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import GHC.Generics (Generic)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertEqual)
import UnitTests.NullaryConstructors (nullaryConstructors)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import qualified Data.Sequence as Seq
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M

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
  , testGroup "To/From JSON representation" $ fmap assertJsonExample jsonExamples
  , testGroup "JSONPath" $ fmap (testCase "-") jsonPath
  , testGroup "Hashable laws" $ fmap (testCase "-") hashableLaws
  , testGroup "Issue #351" $ fmap (testCase "-") issue351
  , testGroup "Nullary constructors" $ fmap (testCase "-") nullaryConstructors
  , testGroup "FromJSONKey" $ fmap (testCase "-") fromJSONKeyAssertions
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
  } deriving (Generic, Show, Eq)

instance FromJSON Wibble

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
jsonEncoding =
  [
  -- https://github.com/bos/aeson/issues/376
    assertEqual "Just Nothing" "null" $ encode (Just Nothing :: Maybe (Maybe Int))
  -- infinities cannot be recovered, null is decoded as NaN
  , assertEqual "inf :: Double" "null" $ encode (Approx $ 1/0 :: Approx Double)
  ]

data Example where
    Example
        :: (Eq a, Show a, ToJSON a, FromJSON a)
        => String -> L.ByteString -> a -> Example

assertJsonExample :: Example -> Test
assertJsonExample (Example name bs val) = testCase name $ do
    assertEqual "encode" bs         (encode val)
    assertEqual "decode" (Just val) (decode bs)

jsonExamples :: [Example]
jsonExamples =
  [
    Example "Either Left" "{\"Left\":1}"  (Left 1 :: Either Int Int)
  , Example "Either Right" "{\"Right\":1}"  (Right 1 :: Either Int Int)
  , Example "Nothing"  "null"  (Nothing :: Maybe Int)
  , Example "Just"  "1"  (Just 1 :: Maybe Int)
  , Example "Proxy Int" "null"  (Proxy :: Proxy Int)
  , Example "Tagged Char Int" "1"  (Tagged 1 :: Tagged Char Int)
#if __GLASGOW_HASKELL__ >= 708
    -- Test Tagged instance is polykinded
  , Example "Tagged 123 Int" "1"  (Tagged 1 :: Tagged 123 Int)
#endif
  , Example "Const Char Int" "\"c\""  (Const 'c' :: Const Char Int)
  , Example "Tuple" "[1,2]"  ((1, 2) :: (Int, Int))
  , Example "NonEmpty" "[1,2,3]"  (1 :| [2, 3] :: NonEmpty Int)
  , Example "Seq" "[1,2,3]"  (Seq.fromList [1, 2, 3] ::  Seq.Seq Int)
  , Example "DList" "[1,2,3]"  (DList.fromList [1, 2, 3] :: DList.DList Int)
  , Example "()" "[]"  ()

  , Example "HashMap Int Int"          "{\"0\":1,\"2\":3}"  (HM.fromList [(0,1),(2,3)] :: HM.HashMap Int Int)
  , Example "Map Int Int"              "{\"0\":1,\"2\":3}"  (M.fromList [(0,1),(2,3)] :: M.Map Int Int)
  , Example "Map (Tagged Int Int) Int" "{\"0\":1,\"2\":3}"  (M.fromList [(Tagged 0,1),(Tagged 2,3)] :: M.Map (Tagged Int Int) Int)
  , Example "Map [Int] Int"            "[[[0],1],[[2],3]]"  (M.fromList [([0],1),([2],3)] :: M.Map [Int] Int)
  , Example "Map [Char] Int"           "{\"ab\":1,\"cd\":3}"  (M.fromList [("ab",1),("cd",3)] :: M.Map [Char] Int)
  , Example "Map [I Char] Int"         "{\"ab\":1,\"cd\":3}"  (M.fromList [(map pure "ab",1),(map pure "cd",3)] :: M.Map [I Char] Int)

  , Example "nan :: Double" "null"  (Approx $ 0/0 :: Approx Double)

  -- Three separate cases, as ordering in HashMap is not defined
  , Example "HashMap Float Int, NaN" "{\"NaN\":1}"  (Approx $ HM.singleton (0/0) 1 :: Approx (HM.HashMap Float Int))
  , Example "HashMap Float Int, Infinity" "{\"Infinity\":1}"  (HM.singleton (1/0) 1 :: HM.HashMap Float Int)
  , Example "HashMap Float Int, +Infinity" "{\"-Infinity\":1}"  (HM.singleton (negate 1/0) 1 :: HM.HashMap Float Int)

  -- Functors
  , Example "Identity Int" "1"  (pure 1 :: Identity Int)

  , Example "Identity Char" "\"x\""      (pure 'x' :: Identity Char)
  , Example "Identity String" "\"foo\""  (pure "foo" :: Identity String)
  , Example "[Identity Char]" "\"xy\""   ([pure 'x', pure 'y'] :: [Identity Char])

  , Example "Maybe Char" "\"x\""              (pure 'x' :: Maybe Char)
  , Example "Maybe String" "\"foo\""          (pure "foo" :: Maybe String)
  , Example "Maybe [Identity Char]" "\"xy\""  (pure [pure 'x', pure 'y'] :: Maybe [Identity Char])

  , Example "Compose I  I  Int" "1"      (pure 1 :: Compose I I   Int)
  , Example "Compose I  [] Int" "[1]"    (pure 1 :: Compose I []  Int)
  , Example "Compose [] I  Int" "[1]"    (pure 1 :: Compose [] I  Int)
  , Example "Compose [] [] Int" "[[1]]"  (pure 1 :: Compose [] [] Int)

  , Example "Compose I  I  Char" "\"x\""    (pure 'x' :: Compose I  I  Char)
  , Example "Compose I  [] Char" "\"x\""    (pure 'x' :: Compose I  [] Char)
  , Example "Compose [] I  Char" "\"x\""    (pure 'x' :: Compose [] I  Char)
  , Example "Compose [] [] Char" "[\"x\"]"  (pure 'x' :: Compose [] [] Char)

  , Example "Compose3 I  I  I  Char" "\"x\""      (pure 'x' :: Compose3 I  I  I  Char)
  , Example "Compose3 I  I  [] Char" "\"x\""      (pure 'x' :: Compose3 I  I  [] Char)
  , Example "Compose3 I  [] I  Char" "\"x\""      (pure 'x' :: Compose3 I  [] I  Char)
  , Example "Compose3 I  [] [] Char" "[\"x\"]"    (pure 'x' :: Compose3 I  [] [] Char)
  , Example "Compose3 [] I  I  Char" "\"x\""      (pure 'x' :: Compose3 [] I  I  Char)
  , Example "Compose3 [] I  [] Char" "[\"x\"]"    (pure 'x' :: Compose3 [] I  [] Char)
  , Example "Compose3 [] [] I  Char" "[\"x\"]"    (pure 'x' :: Compose3 [] [] I  Char)
  , Example "Compose3 [] [] [] Char" "[[\"x\"]]"  (pure 'x' :: Compose3 [] [] [] Char)

  , Example "Compose3' I  I  I  Char" "\"x\""      (pure 'x' :: Compose3' I  I  I  Char)
  , Example "Compose3' I  I  [] Char" "\"x\""      (pure 'x' :: Compose3' I  I  [] Char)
  , Example "Compose3' I  [] I  Char" "\"x\""      (pure 'x' :: Compose3' I  [] I  Char)
  , Example "Compose3' I  [] [] Char" "[\"x\"]"    (pure 'x' :: Compose3' I  [] [] Char)
  , Example "Compose3' [] I  I  Char" "\"x\""      (pure 'x' :: Compose3' [] I  I  Char)
  , Example "Compose3' [] I  [] Char" "[\"x\"]"    (pure 'x' :: Compose3' [] I  [] Char)
  , Example "Compose3' [] [] I  Char" "[\"x\"]"    (pure 'x' :: Compose3' [] [] I  Char)
  , Example "Compose3' [] [] [] Char" "[[\"x\"]]"  (pure 'x' :: Compose3' [] [] [] Char)
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
  , assertEqual "Wibble"
      (Left "Error in $.wibbleInt: expected Int, encountered Boolean")
      (eitherDecode "{\"wibbleString\":\"\",\"wibbleInt\":true}"
         :: Either String Wibble)
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

-------------------------------------------------------------------------------
-- ToJSONKey
-------------------------------------------------------------------------------

newtype MyText = MyText Text
    deriving (FromJSONKey)

newtype MyText' = MyText' Text

instance FromJSONKey MyText' where
    fromJSONKey = fmap MyText' fromJSONKey
    fromJSONKeyList = error "not used"

fromJSONKeyAssertions :: [Assertion]
fromJSONKeyAssertions =
    [ assertIsCoerce  "Text"            (fromJSONKey :: FromJSONKeyFunction Text)
    , assertIsCoerce  "Tagged Int Text" (fromJSONKey :: FromJSONKeyFunction (Tagged Int Text))
    , assertIsCoerce  "MyText"          (fromJSONKey :: FromJSONKeyFunction MyText)

#if __GLASGOW_HASKELL__ >= 710
    , assertIsCoerce' "MyText'"         (fromJSONKey :: FromJSONKeyFunction MyText')
#endif
    ]
  where
    assertIsCoerce _ (FromJSONKeyCoerce _) = pure ()
    assertIsCoerce n _                     = assertFailure n

#if __GLASGOW_HASKELL__ >= 710
    assertIsCoerce' _ (FromJSONKeyCoerce _) = pure ()
    assertIsCoerce' n _                     = pickWithRules (assertFailure n) (pure ())

-- | Pick the first when RULES are enabled, e.g. optimisations are on
pickWithRules
    :: a -- ^ Pick this when RULES are on
    -> a -- ^ use this otherwise
    -> a
pickWithRules _ = id
{-# NOINLINE pickWithRules #-}
{-# RULES "pickWithRules/rule" [0] forall x. pickWithRules x = const x #-}
#endif

------------------------------------------------------------------------------
-- Regressions
------------------------------------------------------------------------------

-- A regression test for: https://github.com/bos/aeson/issues/351
overlappingRegression :: FromJSON a => L.ByteString -> [a]
overlappingRegression bs = fromMaybe [] $ decode bs

issue351 :: [Assertion]
issue351 = [
    assertEqual "Int"  ([1, 2, 3] :: [Int])  $ overlappingRegression "[1, 2, 3]"
  , assertEqual "Char" ("abc"     :: String) $ overlappingRegression "\"abc\""
  , assertEqual "Char" (""        :: String) $ overlappingRegression "[\"a\", \"b\", \"c\"]"
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
