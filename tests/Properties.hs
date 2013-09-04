{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

import Data.Aeson.Encode
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Attoparsec.Number
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.Vector as V
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import Data.Time.Clock (UTCTime(..))
import Data.Time (ZonedTime(..))
import Instances ()
import Types
import Encoders
import Properties.Deprecated (deprecatedTests)
#ifdef GHC_GENERICS
import Data.Int
import qualified Data.Map as Map
#endif


encodeDouble :: Double -> Double -> Bool
encodeDouble num denom
    | isInfinite d || isNaN d = encode (Number (D d)) == "null"
    | otherwise               = (read . L.unpack . encode . Number . D) d == d
  where d = num / denom

encodeInteger :: Integer -> Bool
encodeInteger i = encode (Number (I i)) == L.pack (show i)

toParseJSON :: (Arbitrary a, Eq a) => (Value -> Parser a) -> (a -> Value) -> a -> Bool
toParseJSON parsejson tojson x =
    case parse parsejson . tojson $ x of
      Error _ -> False
      Success x' -> x == x'

roundTrip :: (FromJSON a, ToJSON a) => (a -> a -> Bool) -> a -> a -> Bool
roundTrip eq _ i =
    case fmap fromJSON . L.parse value . encode . toJSON $ i of
      L.Done _ (Success v) -> v `eq` i
      _                    -> False

roundTripEq :: (Eq a, FromJSON a, ToJSON a) => a -> a -> Bool
roundTripEq x y = roundTrip (==) x y

toFromJSON :: (Arbitrary a, Eq a, FromJSON a, ToJSON a) => a -> Bool
toFromJSON x = case fromJSON . toJSON $ x of
                Error _ -> False
                Success x' -> x == x'

modifyFailureProp :: String -> String -> Bool
modifyFailureProp orig added =
    result == Error (added ++ orig)
  where
    parser = const $ modifyFailure (added ++) $ fail orig
    result :: Result ()
    result = parse parser ()

main :: IO ()
main = defaultMain tests

#ifdef GHC_GENERICS
type P6 = Product6 Int Bool String (Approx Double) (Int, Approx Double) ()
type S4 = Sum4 Int8 ZonedTime T.Text (Map.Map String Int)
#endif

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

--------------------------------------------------------------------------------

tests :: [Test]
tests = [
  testGroup "encode" [
      testProperty "encodeDouble" encodeDouble
    , testProperty "encodeInteger" encodeInteger
    ],
  testGroup "roundTrip" [
      testProperty "Bool" $ roundTripEq True
    , testProperty "Double" $ roundTripEq (1 :: Approx Double)
    , testProperty "Int" $ roundTripEq (1::Int)
    , testProperty "Integer" $ roundTripEq (1::Integer)
    , testProperty "String" $ roundTripEq (""::String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Foo" $ roundTripEq (undefined::Foo)
    , testProperty "DotNetTime" $ roundTripEq (undefined :: Approx DotNetTime)
    , testProperty "UTCTime" $ roundTripEq (undefined :: Approx UTCTime)
    , testProperty "ZonedTime" $ roundTripEq (undefined::ZonedTime)
#ifdef GHC_GENERICS
    , testGroup "ghcGenerics" [
        testProperty "OneConstructor" $ roundTripEq OneConstructor
      , testProperty "Product2" $ roundTripEq (undefined :: Product2 Int Bool)
      , testProperty "Product6" $ roundTripEq (undefined :: P6)
      , testProperty "Sum4" $ roundTripEq (undefined :: S4)
      ]
#endif
    ],
  testGroup "toFromJSON" [
      testProperty "Integer" (toFromJSON :: Integer -> Bool)
    , testProperty "Double" (toFromJSON :: Double -> Bool)
    , testProperty "Maybe Integer" (toFromJSON :: Maybe Integer -> Bool)
    , testProperty "Either Integer Double" (toFromJSON :: Either Integer Double -> Bool)
    , testProperty "Either Integer Integer" (toFromJSON :: Either Integer Integer -> Bool)
    ],
  testGroup "deprecated" deprecatedTests,
  testGroup "failure messages" [
      testProperty "modify failure" modifyFailureProp
    ],
  testGroup "template-haskell" [
      testGroup "Nullary" [
          testProperty "string"                (isString                . thNullaryToJSONString)
        , testProperty "2ElemArray"            (is2ElemArray            . thNullaryToJSON2ElemArray)
        , testProperty "TaggedObject"          (isTaggedObjectValue     . thNullaryToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thNullaryToJSONObjectWithSingleField)

        , testGroup "roundTrip" [
              testProperty "string"                (toParseJSON thNullaryParseJSONString                thNullaryToJSONString)
            , testProperty "2ElemArray"            (toParseJSON thNullaryParseJSON2ElemArray            thNullaryToJSON2ElemArray)
            , testProperty "TaggedObject"          (toParseJSON thNullaryParseJSONTaggedObject          thNullaryToJSONTaggedObject)
            , testProperty "ObjectWithSingleField" (toParseJSON thNullaryParseJSONObjectWithSingleField thNullaryToJSONObjectWithSingleField)
          ]
        ]
    , testGroup "SomeType" [
          testProperty "2ElemArray"            (is2ElemArray            . (thSomeTypeToJSON2ElemArray            :: SomeTypeToJSON))
        , testProperty "TaggedObject"          (isTaggedObject          . (thSomeTypeToJSONTaggedObject          :: SomeTypeToJSON))
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . (thSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))

        , testGroup "roundTrip" [
              testProperty "2ElemArray"            (toParseJSON thSomeTypeParseJSON2ElemArray            (thSomeTypeToJSON2ElemArray            :: SomeTypeToJSON))
            , testProperty "TaggedObject"          (toParseJSON thSomeTypeParseJSONTaggedObject          (thSomeTypeToJSONTaggedObject          :: SomeTypeToJSON))
            , testProperty "ObjectWithSingleField" (toParseJSON thSomeTypeParseJSONObjectWithSingleField (thSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))
          ]
      ]
    ]
#ifdef GHC_GENERICS
  , testGroup "GHC-generics" [
        testGroup "Nullary" [
            testProperty "string"                (isString                . gNullaryToJSONString)
          , testProperty "2ElemArray"            (is2ElemArray            . gNullaryToJSON2ElemArray)
          , testProperty "TaggedObject"          (isTaggedObjectValue     . gNullaryToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (isObjectWithSingleField . gNullaryToJSONObjectWithSingleField)
          , testGroup "eq" [
                testProperty "string"                (\n -> gNullaryToJSONString                n == thNullaryToJSONString                n)
              , testProperty "2ElemArray"            (\n -> gNullaryToJSON2ElemArray            n == thNullaryToJSON2ElemArray            n)
              , testProperty "TaggedObject"          (\n -> gNullaryToJSONTaggedObject          n == thNullaryToJSONTaggedObject          n)
              , testProperty "ObjectWithSingleField" (\n -> gNullaryToJSONObjectWithSingleField n == thNullaryToJSONObjectWithSingleField n)
            ]
          , testGroup "roundTrip" [
              testProperty "string"                (toParseJSON gNullaryParseJSONString                gNullaryToJSONString)
            , testProperty "2ElemArray"            (toParseJSON gNullaryParseJSON2ElemArray            gNullaryToJSON2ElemArray)
            , testProperty "TaggedObject"          (toParseJSON gNullaryParseJSONTaggedObject          gNullaryToJSONTaggedObject)
            , testProperty "ObjectWithSingleField" (toParseJSON gNullaryParseJSONObjectWithSingleField gNullaryToJSONObjectWithSingleField)
            ]
          ]
    , testGroup "SomeType" [
          testProperty "2ElemArray"            (is2ElemArray            . (gSomeTypeToJSON2ElemArray            :: SomeTypeToJSON))
        , testProperty "TaggedObject"          (isTaggedObject          . (gSomeTypeToJSONTaggedObject          :: SomeTypeToJSON))
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . (gSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))

        , testGroup "eq" [
              testProperty "2ElemArray"            (\n -> (gSomeTypeToJSON2ElemArray            :: SomeTypeToJSON) n == thSomeTypeToJSON2ElemArray            n)
            , testProperty "TaggedObject"          (\n -> (gSomeTypeToJSONTaggedObject          :: SomeTypeToJSON) n == thSomeTypeToJSONTaggedObject          n)
            , testProperty "ObjectWithSingleField" (\n -> (gSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON) n == thSomeTypeToJSONObjectWithSingleField n)
          ]
        , testGroup "roundTrip" [
            testProperty "2ElemArray"            (toParseJSON gSomeTypeParseJSON2ElemArray            (gSomeTypeToJSON2ElemArray            :: SomeTypeToJSON))
          , testProperty "TaggedObject"          (toParseJSON gSomeTypeParseJSONTaggedObject          (gSomeTypeToJSONTaggedObject          :: SomeTypeToJSON))
          , testProperty "ObjectWithSingleField" (toParseJSON gSomeTypeParseJSONObjectWithSingleField (gSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))
          ]
      ]
    ]
#endif
  ]
