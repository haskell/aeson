{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

-- For Data.Aeson.Types.camelTo
{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module UnitTests
    (
      ioTests
    , tests
    , withEmbeddedJSONTest
    ) where

import Prelude.Compat

import Control.Monad (forM, forM_, when)
import Data.Aeson ((.=), (.:), (.:?), (.:!), FromJSON(..), ToJSON1(..), decode, eitherDecode, encode, fromJSON, genericParseJSON, genericToEncoding, genericToJSON, object, withObject, withEmbeddedJSON)
import Data.Aeson.Types (JSONPathElement(..), formatError)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.TH (deriveJSON, deriveToJSON, deriveToJSON1)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Aeson.Types
  ( Options(..), Result(Success, Error), ToJSON(..)
  , Value(..), camelTo, camelTo2
  , defaultOptions, formatPath, formatRelativePath, omitNothingFields, parse)
import Data.Char (toUpper, GeneralCategory(Control,Surrogate), generalCategory)
import Data.HashMap.Strict (HashMap)
import Data.Kind (Type)
import Data.List (isSuffixOf)
import Data.Scientific (Scientific, scientific)
import GHC.Generics (Generic)
#if __GLASGOW_HASKELL__ >= 806
import GHC.Generics.Generically (Generically (..))
#endif
import Instances ()
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, assertEqual, testCase)
import Text.Printf (printf)
import qualified Data.ByteString.Base16.Lazy as LBase16
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.Encoding as TLE
import qualified ErrorMessages
import qualified SerializationFormatSpec

import Regression.Issue351
import Regression.Issue571
import Regression.Issue687
import Regression.Issue967
import UnitTests.OmitNothingFieldsNote
import UnitTests.FromJSONKey
import UnitTests.Hashable
import UnitTests.KeyMapInsertWith
import UnitTests.MonadFix
import UnitTests.NoThunks
import UnitTests.NullaryConstructors (nullaryConstructors)
import UnitTests.OptionalFields (optionalFields)
import UnitTests.UTCTime

roundTripCamel :: String -> Assertion
roundTripCamel name = assertEqual "" name (camelFrom '_' $ camelTo '_' name)

roundTripCamel2 :: String -> Assertion
roundTripCamel2 name = assertEqual "" name (camelFrom '_' $ camelTo2 '_' name)

camelFrom :: Char -> String -> String
camelFrom c s = case split c s of
  p:ps ->  concat $ p : map capitalize ps
  _    -> s -- shouldn't happen?
  where
    split c' s' = map L.unpack $ L.split c' $ L.pack s'
    capitalize t = toUpper (head t) : tail t


data Wibble = Wibble {
    wibbleString :: String
  , wibbleInt :: Int
  } deriving (Generic, Show, Eq)

instance FromJSON Wibble

#if __GLASGOW_HASKELL__ >= 806
deriving via Generically Wibble instance ToJSON Wibble
#else
instance ToJSON Wibble where
    toJSON     = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions
#endif

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

-- Non identifier keys should be escaped & enclosed in brackets
formatErrorExample :: Assertion
formatErrorExample =
  let rhs = formatError [Index 0, Key "foo", Key "bar", Key "a.b.c", Key "", Key "'\\", Key "end"] "error msg"
      lhs = "Error in $[0].foo.bar['a.b.c']['']['\\'\\\\'].end: error msg"
  in assertEqual "formatError example" lhs rhs

formatPathExample :: Assertion
formatPathExample =
  let rhs = formatPath [Key "x", Index 0]
      lhs = "$.x[0]"
  in assertEqual "formatPath example" lhs rhs

formatRelativePathExample :: Assertion
formatRelativePathExample =
  let rhs = formatRelativePath [Key "x", Index 0]
      lhs = ".x[0]"
  in assertEqual "formatRelativePath example" lhs rhs

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

  , assertEqual ".:? not-present" (Just (T2 Nothing))   (decode ex1 :: Maybe T2)
  , assertEqual ".:? 42"          (Just (T2 (Just 42))) (decode ex2 :: Maybe T2)
  , assertEqual ".:? null"        (Just (T2 Nothing))   (decode ex3 :: Maybe T2)

  , assertEqual ".:! not-present" (Just (T3 Nothing))   (decode ex1 :: Maybe T3)
  , assertEqual ".:! 42"          (Just (T3 (Just 42))) (decode ex2 :: Maybe T3)
  , assertEqual ".:! null"        Nothing               (decode ex3 :: Maybe T3)
  ]
  where ex1 = "{}"
        ex2 = "{\"value\": 42 }"
        ex3 = "{\"value\": null }"

------------------------------------------------------------------------------
-- Check that an alternative way to construct objects works
------------------------------------------------------------------------------

objectConstruction :: [Assertion]
objectConstruction = [
    assertEqual "Equal objects constructed differently" recommended notRecommended
  ]
  where
    recommended = object ["foo" .= True, "bar" .= (-1 :: Int)]
    notRecommended = Object (mconcat ["foo" .= True, "bar" .= (-1 :: Int)])

------------------------------------------------------------------------------
-- Comparison between bytestring and text encoders
------------------------------------------------------------------------------

encoderComparisonTests :: IO TestTree
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

-- A regression test for: https://github.com/bos/aeson/pull/477
unescapeString :: Assertion
unescapeString = do
  assertEqual "Basic escaping"
     (Right ("\" / \\ \b \f \n \r \t" :: String))
     (eitherDecode "\"\\\" \\/ \\\\ \\b \\f \\n \\r \\t\"")

  forM_ [minBound .. maxBound :: Char] $ \ c -> do
    let s = LT.pack [c]

    assertEqual (printf "UTF-16 encoded '\\x%X'" c) (Right s) (eitherDecode $ utf16Char s)

    when (notEscapeControlOrSurrogate c) $
        assertEqual (printf "UTF-8 encode '\\x%X'" c) (Right s) (eitherDecode $ utf8Char s)

  where
    utf16Char = formatString . LBase16.encode . LT.encodeUtf16BE
    formatString s
      | L.length s == 4 = L.concat ["\"\\u", s, "\""]
      | L.length s == 8 =
          L.concat ["\"\\u", L.take 4 s, "\\u", L.drop 4 s, "\""]
      | otherwise = error "unescapeString: can't happen"

    utf8Char s = L.concat ["\"", LT.encodeUtf8 s, "\""]

    notEscapeControlOrSurrogate '"'  = False
    notEscapeControlOrSurrogate '\\' = False
    notEscapeControlOrSurrogate c = case generalCategory c of
      Control -> False
      Surrogate -> False
      _ -> True



-- A regression test for: https://github.com/bos/aeson/pull/455
data Foo a = FooNil | FooCons (Foo Int)
deriveToJSON  defaultOptions ''Foo
deriveToJSON1 defaultOptions ''Foo

pr455 :: Assertion
pr455 = assertEqual "FooCons FooNil"
          (toJSON foo) (liftToJSON undefined undefined undefined foo)
  where
    foo :: Foo Int
    foo = FooCons FooNil

showOptions :: Assertion
showOptions =
    assertEqual
        "Show Options"
        (  "Options {"
        ++   "fieldLabelModifier =~ \"exampleField\""
        ++ ", constructorTagModifier =~ \"ExampleConstructor\""
        ++ ", allNullaryToStringTag = True"
        ++ ", nullaryToObject = False"
        ++ ", omitNothingFields = False"
        ++ ", allowOmittedFields = True"
        ++ ", sumEncoding = TaggedObject {tagFieldName = \"tag\", contentsFieldName = \"contents\"}"
        ++ ", unwrapUnaryRecords = False"
        ++ ", tagSingleConstructors = False"
        ++ ", rejectUnknownFields = False"
        ++ "}")
        (show defaultOptions)

newtype SingleMaybeField = SingleMaybeField { smf :: Maybe Int }
  deriving (Eq, Show, Generic)
deriveJSON defaultOptions{omitNothingFields=True,unwrapUnaryRecords=True} ''SingleMaybeField

singleMaybeField :: [TestTree]
singleMaybeField = do
  (gName, gToJSON, gToEncoding, gFromJSON) <-
    [ ("generic", genericToJSON opts, genericToEncoding opts, parse (genericParseJSON opts))
    , ("th", toJSON, toEncoding, fromJSON) ]
  return $
    testCase gName $ do
      assertEqual "toJSON"     Null (gToJSON v)
      assertEqual "toEncoding" (toEncoding (gToJSON v)) (gToEncoding v)
      assertEqual "fromJSON"   (Success v) (gFromJSON Null)
  where
    v = SingleMaybeField Nothing
    opts = defaultOptions{omitNothingFields=True,unwrapUnaryRecords=True}


newtype EmbeddedJSONTest = EmbeddedJSONTest Int
  deriving (Eq, Show)

instance FromJSON EmbeddedJSONTest where
  parseJSON =
    withObject "Object" $ \o ->
      EmbeddedJSONTest <$> (o .: "prop" >>= withEmbeddedJSON "Quoted Int" parseJSON)

withEmbeddedJSONTest :: Assertion
withEmbeddedJSONTest =
  assertEqual "Unquote embedded JSON" (Right $ EmbeddedJSONTest 1) (eitherDecode "{\"prop\":\"1\"}")

-- Regression test for https://github.com/bos/aeson/issues/627
newtype SingleFieldCon = SingleFieldCon Int deriving (Eq, Show, Generic)

instance FromJSON SingleFieldCon where
  parseJSON = genericParseJSON defaultOptions{unwrapUnaryRecords=True}
  -- This option should have no effect on this type

singleFieldCon :: Assertion
singleFieldCon =
  assertEqual "fromJSON" (Right (SingleFieldCon 0)) (eitherDecode "0")

newtype UnknownFields = UnknownFields { knownField :: Int }
  deriving (Eq, Show, Generic)
deriveJSON defaultOptions{rejectUnknownFields=True} ''UnknownFields

newtype UnknownFieldsTag = UnknownFieldsTag { tag :: Int }
  deriving (Eq, Show, Generic)
deriveJSON defaultOptions{rejectUnknownFields=True} ''UnknownFieldsTag

newtype UnknownFieldsUnaryTagged = UnknownFieldsUnaryTagged { knownFieldUnaryTagged :: Int }
  deriving (Eq, Show, Generic)
deriveJSON defaultOptions{tagSingleConstructors=True,rejectUnknownFields=True} ''UnknownFieldsUnaryTagged

data UnknownFieldsSum
  = UnknownFields1 { knownField1 :: Int }
  | UnknownFields2 { knownField2 :: Int }
  deriving (Eq, Show, Generic)
deriveJSON defaultOptions{rejectUnknownFields=True} ''UnknownFieldsSum

unknownFields :: [TestTree]
unknownFields = concat
    [ testsUnary
        "unary-unknown"
        (object [("knownField", Number 1), ("unknownField", Number 1)])
        (Error "nknown fields: [\"unknownField\"]" :: Result UnknownFields)
    , testsUnary
        "unary-unknown-tag"
        (object [("knownField", Number 1), ("tag", String "UnknownFields")])
        (Error "nknown fields: [\"tag\"]" :: Result UnknownFields)
    , testsUnaryTag
        "unary-explicit-tag"
        (object [("tag", Number 1)])
        (Success $ UnknownFieldsTag 1)
    , testsSum
        "sum-tag"
        (object [("knownField1", Number 1), ("tag", String "UnknownFields1")])
        (Success $ UnknownFields1 1)
    , testsSum
        "sum-unknown-in-branch"
        (object [("knownField1", Number 1), ("knownField2", Number 1), ("tag", String "UnknownFields1")])
        (Error "nknown fields: [\"knownField2\"]" :: Result UnknownFieldsSum)
    , testsSum
        "sum-unknown"
        (object [("knownField1", Number 1), ("unknownField", Number 1), ("tag", String "UnknownFields1")])
        (Error "nknown fields: [\"unknownField\"]" :: Result UnknownFieldsSum)
    , testsTagged
        "unary-tagged"
        (object [("knownFieldUnaryTagged", Number 1), ("tag", String "UnknownFieldsUnaryTagged")])
        (Success $ UnknownFieldsUnaryTagged 1)
    , -- Just a case to verify that the tag isn't optional, this is likely already tested by other unit tests
      testsTagged
        "unary-tagged-notag"
        (object [("knownFieldUnaryTagged", Number 1)])
        (Error "key \"tag\" not found" :: Result UnknownFieldsUnaryTagged)
    , testsTagged
        "unary-tagged-unknown"
        (object [ ("knownFieldUnaryTagged", Number 1), ("unknownField", Number 1)
                , ("tag", String "UnknownFieldsUnaryTagged")])
        (Error "nknown fields: [\"unknownField\"]" :: Result UnknownFieldsUnaryTagged)
    ]
    where
        opts = defaultOptions{rejectUnknownFields=True}
        taggedOpts = opts{tagSingleConstructors=True}
        assertApprox :: (Show a, Eq a) => Result a -> Result a -> IO ()
        assertApprox (Error expected) (Error actual) | expected `isSuffixOf` actual = return ()
        assertApprox expected actual = assertEqual "fromJSON" expected actual
        testsBase :: (Show a, Eq a) => (Value -> Result a) -> (Value -> Result a)
                                    -> String -> Value -> Result a -> [TestTree]
        testsBase th g name value expected =
            [ testCase (name ++ "-th") $ assertApprox expected (th value)
            , testCase (name ++ "-generic") $ assertApprox expected (g value)
            ]
        testsUnary :: String -> Value -> Result UnknownFields -> [TestTree]
        testsUnary = testsBase fromJSON (parse (genericParseJSON opts))
        testsUnaryTag :: String -> Value -> Result UnknownFieldsTag -> [TestTree]
        testsUnaryTag = testsBase fromJSON (parse (genericParseJSON opts))
        testsSum :: String -> Value -> Result UnknownFieldsSum -> [TestTree]
        testsSum = testsBase fromJSON (parse (genericParseJSON opts))
        testsTagged :: String -> Value -> Result UnknownFieldsUnaryTagged -> [TestTree]
        testsTagged = testsBase fromJSON (parse (genericParseJSON taggedOpts))

{-
testParser :: (Eq a, Show a)
           => String -> Parser a -> S.ByteString -> Either String a -> TestTree
testParser name json_ s expected =
  testCase name (parseOnly json_ s @?= expected)

keyOrdering :: [TestTree]
keyOrdering =
  [ testParser "json" json
      "{\"k\":true,\"k\":false}" $
      Right (Object (KM.fromList [("k", Bool True)]))
  , testParser "jsonLast" jsonLast
      "{\"k\":true,\"k\":false}" $
      Right (Object (KM.fromList [("k", Bool False)]))
  , testParser "jsonAccum" jsonAccum
      "{\"k\":true,\"k\":false}" $
      Right (Object (KM.fromList [("k", Array (Vector.fromList [Bool True, Bool False]))]))
  , testParser "jsonNoDup" jsonNoDup
      "{\"k\":true,\"k\":false}" $
      Left "Failed reading: found duplicate key: \"k\""

  , testParser "json'" json'
      "{\"k\":true,\"k\":false}" $
      Right (Object (KM.fromList [("k", Bool True)]))
  , testParser "jsonLast'" jsonLast'
      "{\"k\":true,\"k\":false}" $
      Right (Object (KM.fromList [("k", Bool False)]))
  , testParser "jsonAccum'" jsonAccum'
      "{\"k\":true,\"k\":false}" $
      Right (Object (KM.fromList [("k", Array (Vector.fromList [Bool True, Bool False]))]))
  , testParser "jsonNoDup'" jsonNoDup'
      "{\"k\":true,\"k\":false}" $
      Left "Failed reading: found duplicate key: \"k\""
  ]
-}

ratioDenominator0 :: Assertion
ratioDenominator0 =
  assertEqual "Ratio with denominator 0"
    (Left "Error in $: Ratio denominator was 0")
    (eitherDecode "{ \"numerator\": 1, \"denominator\": 0 }" :: Either String Rational)

rationalNumber :: Assertion
rationalNumber =
  assertEqual "Ratio with denominator 0"
    (Right 1.37)
    (eitherDecode "1.37" :: Either String Rational)

bigRationalDecoding :: Assertion
bigRationalDecoding =
  assertEqual "Decoding an Integer with a large exponent should fail"
    (Left "Error in $: parsing Ratio failed, found a number with exponent 2000, but it must not be greater than 1024 or less than -1024")
    ((eitherDecode :: L.ByteString -> Either String Rational) "1e2000")

smallRationalDecoding :: Assertion
smallRationalDecoding =
  assertEqual "Decoding an Integer with a large exponent should fail"
    (Left "Error in $: parsing Ratio failed, found a number with exponent -2000, but it must not be greater than 1024 or less than -1024")
    ((eitherDecode :: L.ByteString -> Either String Rational) "1e-2000")


bigScientificExponent :: Assertion
bigScientificExponent =
  assertEqual "Encoding an integral scientific with a large exponent should normalize it"
    "1.0e2000"
    (encode (scientific 1 2000 :: Scientific))

bigIntegerDecoding :: Assertion
bigIntegerDecoding =
  assertEqual "Decoding an Integer with a large exponent should fail"
    (Left "Error in $: parsing Integer failed, found a number with exponent 2000, but it must not be greater than 1024")
    ((eitherDecode :: L.ByteString -> Either String Integer) "1e2000")

bigNaturalDecoding :: Assertion
bigNaturalDecoding =
  assertEqual "Decoding a Natural with a large exponent should fail"
    (Left "Error in $: parsing Natural failed, found a number with exponent 2000, but it must not be greater than 1024")
    ((eitherDecode :: L.ByteString -> Either String Natural) "1e2000")

bigIntegerKeyDecoding :: Assertion
bigIntegerKeyDecoding =
  assertEqual "Decoding an Integer key with a large exponent should fail"
    (Left "Error in $['1e2000']: parsing Integer failed, found a number with exponent 2000, but it must not be greater than 1024")
    ((eitherDecode :: L.ByteString -> Either String (HashMap Integer Value)) "{ \"1e2000\": null }")

bigNaturalKeyDecoding :: Assertion
bigNaturalKeyDecoding =
  assertEqual "Decoding an Integer key with a large exponent should fail"
    (Left "Error in $['1e2000']: found a number with exponent 2000, but it must not be greater than 1024")
    ((eitherDecode :: L.ByteString -> Either String (HashMap Natural Value)) "{ \"1e2000\": null }")

-- A regression test for: https://github.com/bos/aeson/issues/757
type family Fam757 :: Type -> Type
type instance Fam757 = Maybe
newtype Newtype757 a = MkNewtype757 (Fam757 a)
deriveToJSON1 defaultOptions ''Newtype757

-------------------------------------------------------------------------------
-- Tests trees
-------------------------------------------------------------------------------

ioTests :: IO [TestTree]
ioTests = do
  enc <- encoderComparisonTests
  return [enc]

tests :: TestTree
tests = testGroup "unit" [
    testGroup "SerializationFormatSpec" SerializationFormatSpec.tests
  , testGroup "ErrorMessages" ErrorMessages.tests
  , testGroup "camelCase" [
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
      testCase "goodProducer" goodProducer
    ]
  , utcTimeTests
  , testGroup "formatError" [
      testCase "example 1" formatErrorExample
    ]
  , testGroup ".:, .:?, .:!" $ fmap (testCase "-") dotColonMark
  , hashableLaws
  , testGroup "Object construction" $ fmap (testCase "-") objectConstruction
  , testGroup "Nullary constructors" $ fmap (testCase "-") nullaryConstructors
  , fromJSONKeyTests
  , optionalFields
  , testCase "PR #455" pr455
  , testCase "Unescape string (PR #477)" unescapeString
  , testCase "Show Options" showOptions
  , testGroup "SingleMaybeField" singleMaybeField
  , testCase "withEmbeddedJSON" withEmbeddedJSONTest
  , testCase "SingleFieldCon" singleFieldCon
  , testGroup "UnknownFields" unknownFields
  -- , testGroup "Ordering of object keys" keyOrdering
  , testCase "Ratio with denominator 0" ratioDenominator0
  , testCase "Rational parses number"   rationalNumber
  , testCase "Big rational"             bigRationalDecoding
  , testCase "Small rational"           smallRationalDecoding
  , testCase "Big scientific exponent" bigScientificExponent
  , testCase "Big integer decoding" bigIntegerDecoding
  , testCase "Big natural decoding" bigNaturalDecoding
  , testCase "Big integer key decoding" bigIntegerKeyDecoding
  , testGroup "QQ.Simple"
    [ testCase "example" $
      assertEqual "" (object ["foo" .= True]) [aesonQQ| {"foo": true } |]
    ]
  , monadFixTests
  , issue351
  , issue571
  , issue687
  , issue967
  , keyMapInsertWithTests
  , omitNothingFieldsNoteTests
  , noThunksTests
  ]
