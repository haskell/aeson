{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

import Control.Monad (forM)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Char (toUpper)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (Assertion, assertFailure, assertEqual)
import Test.QuickCheck (Arbitrary(..), Property, (===))
import qualified Data.Vector as V
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.HashMap.Strict as H
import Data.Time
import Instances ()
import Types
import Encoders
import Data.Int
import qualified Data.Map as Map
import Properties.ZonedTime (zonedTimeToJSON)

roundTripCamel :: String -> Assertion
roundTripCamel name = assertEqual "" name (camelFrom '_' $ camelTo '_' name)


  where
    camelFrom c s = let (p:ps) = split c s
                    in concat $ p : map capitalize ps
    split c s = map L.unpack $ L.split c $ L.pack s
    capitalize t = toUpper (head t) : tail t

encodeDouble :: Double -> Double -> Property
encodeDouble num denom
    | isInfinite d || isNaN d = encode d === "null"
    | otherwise               = (read . L.unpack . encode) d === d
  where d = num / denom

encodeInteger :: Integer -> Property
encodeInteger i = encode i === L.pack (show i)

toParseJSON :: (Arbitrary a, Eq a, Show a) =>
               (Value -> Parser a) -> (a -> Value) -> a -> Property
toParseJSON parsejson tojson x =
    case parse parsejson . tojson $ x of
      Error path msg -> failure "parse" (formatError path msg) x
      Success x'     -> x === x'

roundTrip :: (FromJSON a, ToJSON a, Show a) =>
             (a -> a -> Property) -> a -> a -> Property
roundTrip eq _ i =
    case fmap fromJSON . L.parse value . encode . toJSON $ i of
      L.Done _ (Success v)      -> v `eq` i
      L.Done _ (Error path err) -> failure "fromJSON" (formatError path err) i
      L.Fail _ _ err            -> failure "parse" err i

roundTripEq :: (Eq a, FromJSON a, ToJSON a, Show a) => a -> a -> Property
roundTripEq x y = roundTrip (===) x y

toFromJSON :: (Arbitrary a, Eq a, FromJSON a, ToJSON a, Show a) => a -> Property
toFromJSON x = case fromJSON (toJSON x) of
                Error path err -> failure "fromJSON" (formatError path err) x
                Success x'     -> x === x'

modifyFailureProp :: String -> String -> Bool
modifyFailureProp orig added =
    result == Error [] (added ++ orig)
  where
    parser = const $ modifyFailure (added ++) $ fail orig
    result :: Result ()
    result = parse parser ()

main :: IO ()
main = do
    comparisonTest <- encoderComparisonTests
    defaultMain (comparisonTest : tests)

type P6 = Product6 Int Bool String (Approx Double) (Int, Approx Double) ()
type S4 = Sum4 Int8 ZonedTime T.Text (Map.Map String Int)

--------------------------------------------------------------------------------
-- Value properties
--------------------------------------------------------------------------------

isString :: Value -> Bool
isString (String _) = True
isString _          = False

is2ElemArray :: Value -> Bool
is2ElemArray (Array v) = V.length v == 2 && isString (V.head v)
is2ElemArray _         = False

isTaggedObjectValue :: Value -> Bool
isTaggedObjectValue (Object obj) = "tag"      `H.member` obj &&
                                   "contents" `H.member` obj
isTaggedObjectValue _            = False

isTaggedObject :: Value -> Bool
isTaggedObject (Object obj) = "tag" `H.member` obj
isTaggedObject _            = False

isObjectWithSingleField :: Value -> Bool
isObjectWithSingleField (Object obj) = H.size obj == 1
isObjectWithSingleField _            = False

isEmptyArray :: Value -> Bool
isEmptyArray (Array arr) = V.null arr
isEmptyArray _ = False

isOmitEmptyContents :: Value -> Bool
isOmitEmptyContents val = case val of
    Object obj -> 
        "tag" `H.member` obj && case H.lookup "contents" obj of
            Just con -> not $ isEmptyArray con
            Nothing  -> True
    _ -> False

--------------------------------------------------------------------------------

tests :: [Test]
tests = [
  testGroup "encode" [
      testProperty "encodeDouble" encodeDouble
    , testProperty "encodeInteger" encodeInteger
    ]
  , testGroup "camelCase" [
      testCase "camelTo" $ roundTripCamel "aName"
    , testCase "camelTo" $ roundTripCamel "another"
    , testCase "camelTo" $ roundTripCamel "someOtherName"
    ]
  , testGroup "roundTrip" [
      testProperty "Bool" $ roundTripEq True
    , testProperty "Double" $ roundTripEq (1 :: Approx Double)
    , testProperty "Int" $ roundTripEq (1 :: Int)
    , testProperty "Integer" $ roundTripEq (1 :: Integer)
    , testProperty "String" $ roundTripEq ("" :: String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Foo" $ roundTripEq (undefined :: Foo)
    , testProperty "DotNetTime" $ roundTripEq (undefined :: DotNetTime)
    , testProperty "UTCTime" $ roundTripEq (undefined :: UTCTime)
    , testProperty "ZonedTime" $ roundTripEq (undefined :: ZonedTime)
    , testGroup "ghcGenerics" [
        testProperty "OneConstructor" $ roundTripEq OneConstructor
      , testProperty "Product2" $ roundTripEq (undefined :: Product2 Int Bool)
      , testProperty "Product6" $ roundTripEq (undefined :: P6)
      , testProperty "Sum4" $ roundTripEq (undefined :: S4)
      ]
    ]
  , testGroup "toFromJSON" [
      testProperty "Integer" (toFromJSON :: Integer -> Property)
    , testProperty "Double" (toFromJSON :: Double -> Property)
    , testProperty "Maybe Integer" (toFromJSON :: Maybe Integer -> Property)
    , testProperty "Either Integer Double" (toFromJSON :: Either Integer Double -> Property)
    , testProperty "Either Integer Integer" (toFromJSON :: Either Integer Integer -> Property)
    , zonedTimeToJSON
    ]
  , testGroup "failure messages" [
      testProperty "modify failure" modifyFailureProp
    ]
  , testGroup "template-haskell" [
      testGroup "Nullary" [
          testProperty "string" (isString . thNullaryToJSONString)
        , testProperty "2ElemArray" (is2ElemArray . thNullaryToJSON2ElemArray)
        , testProperty "TaggedObject" (isTaggedObjectValue . thNullaryToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thNullaryToJSONObjectWithSingleField)
        , testProperty "OmitEmptyContents"     (isOmitEmptyContents     . thNullaryToJSONOmitEmptyContents)

        , testGroup "roundTrip" [
            testProperty "string" (toParseJSON thNullaryParseJSONString thNullaryToJSONString)
          , testProperty "2ElemArray" (toParseJSON thNullaryParseJSON2ElemArray thNullaryToJSON2ElemArray)
          , testProperty "TaggedObject" (toParseJSON thNullaryParseJSONTaggedObject thNullaryToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (toParseJSON thNullaryParseJSONObjectWithSingleField thNullaryToJSONObjectWithSingleField)
          , testProperty "OmitEmptyContents" (toParseJSON thNullaryParseJSONOmitEmptyContents thNullaryToJSONOmitEmptyContents)
          ]
      ]
    , testGroup "SomeType" [
        testProperty "2ElemArray" (is2ElemArray . (thSomeTypeToJSON2ElemArray :: SomeTypeToJSON))
      , testProperty "TaggedObject" (isTaggedObject . (thSomeTypeToJSONTaggedObject :: SomeTypeToJSON))
      , testProperty "ObjectWithSingleField" (isObjectWithSingleField . (thSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))
      , testGroup "roundTrip" [
          testProperty "2ElemArray" (toParseJSON thSomeTypeParseJSON2ElemArray (thSomeTypeToJSON2ElemArray :: SomeTypeToJSON))
        , testProperty "TaggedObject" (toParseJSON thSomeTypeParseJSONTaggedObject (thSomeTypeToJSONTaggedObject :: SomeTypeToJSON))
        , testProperty "ObjectWithSingleField" (toParseJSON thSomeTypeParseJSONObjectWithSingleField (thSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))
        ]
     , testGroup "Approx" [
          testProperty "string"                (isString                . (thApproxToJSONUnwrap  ::ApproxToJSON))
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . (thApproxToJSONDefault ::ApproxToJSON))
        , testGroup "roundTrip" [
              testProperty "string"                (toParseJSON thApproxParseJSONUnwrap  (thApproxToJSONUnwrap  ::ApproxToJSON))
            , testProperty "ObjectWithSingleField" (toParseJSON thApproxParseJSONDefault (thApproxToJSONDefault ::ApproxToJSON))
          ]
        ]
      ]
    , testGroup "OneConstructor" [
        testProperty "OmitEmptyContents" (isEmptyArray . thOneConstructorToJSONOmitEmptyContents)
      , testGroup "roundTrip" [
            testProperty "OmitEmptyContents" (toParseJSON thOneConstructorParseJSONOmitEmptyContents thOneConstructorToJSONOmitEmptyContents)
        ]
      ]
    ]
  ]

------------------------------------------------------------------------------
-- Comparison between bytestring and text encoders
------------------------------------------------------------------------------

encoderComparisonTests :: IO Test
encoderComparisonTests = do
    encoderTests <- forM testFiles $ \file0 -> do
        let file = "benchmarks/json-data/" ++ file0
        return $ testCase file $ do
            inp <- L.readFile file
            case eitherDecode inp of
              Left  err -> assertFailure $ "Decoding failure: " ++ err
              Right val -> assertEqual "" (encode val) (encodeViaText val)
    return $ testGroup "Compare bytestring and text encoders" encoderTests
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
