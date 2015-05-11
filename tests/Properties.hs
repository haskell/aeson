{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

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
import Test.QuickCheck (Arbitrary(..))
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
import Properties.Deprecated (deprecatedTests)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

#ifdef GHC_GENERICS
import Data.Int
import qualified Data.Map as Map
#endif

#if !MIN_VERSION_time(1,5,0)
import System.Locale (defaultTimeLocale, dateTimeFmt)
#endif


roundTripCamel :: String -> Assertion
roundTripCamel name = assertEqual "" name (camelFrom '_' $ camelTo '_' name)


  where
    camelFrom c s = let (p:ps) = split c s
                    in concat $ p : map capitalize ps
    split c s = map L.unpack $ L.split c $ L.pack s
    capitalize t = toUpper (head t) : tail t

encodeDouble :: Double -> Double -> Bool
encodeDouble num denom
    | isInfinite d || isNaN d = encode d == "null"
    | otherwise               = (read . L.unpack . encode) d == d
  where d = num / denom

encodeInteger :: Integer -> Bool
encodeInteger i = encode i == L.pack (show i)

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

zonedTimeToJSON :: ZonedTime -> Bool
zonedTimeToJSON t = and $
    toFromJSON' "%FT%T%QZ" t (clearTimeZone t) :  -- (javascript new Date().toISOString())
    toFromJSON' "%F %T%Q%z" t t :   -- (postgres)
    toFromJSON' "%F %T%Q %Z" t t :   -- (time's Show format)
    toFromJSON' "%FT%T%Q%z" t t :
    toFromJSON' "%Y-%mT%T%Q" t (clearTimeZone . clearDay $ t) :
    toFromJSON' "%Y-%mT%R" t (clearTimeZone . clearDay . clearSeconds $ t) :
    toFromJSON' "%Y-%mT%T" t (clearTimeZone . clearDay $ t) :
    toFromJSON' "%Y-%mT%T%QZ" t (clearTimeZone . clearDay $ t) :
    toFromJSON' "%Y-%mT%T%Q%z" t (clearDay t) :
    toFromJSON' "%YT%T%Q" t (clearTimeZone . clearMonth . clearDay $ t) :
    toFromJSON' "%YT%R" t (clearTimeZone . clearMonth . clearDay . clearSeconds $ t) :
    toFromJSON' "%YT%T" t (clearTimeZone . clearMonth . clearDay $ t) :
    toFromJSON' "%YT%T%QZ" t (clearTimeZone . clearMonth . clearDay $ t) :
    toFromJSON' "%YT%T%Q%z" t (clearMonth . clearDay $ t) :
    toFromJSON' "%FT%T%Q" t (clearTimeZone t) :
    toFromJSON' "%FT%R" t (clearTimeZone . clearSeconds $ t) :
    toFromJSON' "%FT%T" t (clearTimeZone t) :
    toFromJSON' (dateTimeFmt defaultTimeLocale) t t :
    []
  where
    toJSON' :: String -> ZonedTime -> Value
    toJSON' format = toJSON . formatTime defaultTimeLocale format

    toFromJSON' :: String -> ZonedTime -> ZonedTime -> Bool
    toFromJSON' format t_ t_' = case fromJSON . toJSON' format $ t_ of
                                  Error msg -> error' msg
                                  Success t_'' -> if t_'' == t_' then True else error' (show t_'')
      where
        error' :: String -> Bool
        error' msg = unsafePerformIO (hPutStrLn stderr . ("zonedTimeToJSON: " ++) $ show
                                       (format, t_, toJSON' format t_, t_', msg))
            `seq` False

clearTimeZone :: ZonedTime -> ZonedTime
clearTimeZone t = t { zonedTimeZone = TimeZone 0 False "" }

clearMonth :: ZonedTime -> ZonedTime
clearMonth = f
  where
    f zt = zt { zonedTimeToLocalTime = g $ zonedTimeToLocalTime zt }
    g lt = lt { localDay = h $ localDay lt }

    h :: Day -> Day
    h = maybe (error "clearMonth") id . parseTime defaultTimeLocale "%Y-%m-%d" . formatTime defaultTimeLocale "%Y-01-%d"

clearDay :: ZonedTime -> ZonedTime
clearDay = f
  where
    f zt = zt { zonedTimeToLocalTime = g $ zonedTimeToLocalTime zt }
    g lt = lt { localDay = h $ localDay lt }

    h :: Day -> Day
    h = maybe (error "clearDay") id . parseTime defaultTimeLocale "%Y-%m-%d" . formatTime defaultTimeLocale "%Y-%m-01"

clearSeconds :: ZonedTime -> ZonedTime
clearSeconds = f
  where
    f zt = zt { zonedTimeToLocalTime = g $ zonedTimeToLocalTime zt }
    g lt = lt { localTimeOfDay = h $ localTimeOfDay lt }

    h :: TimeOfDay -> TimeOfDay
    h = maybe (error "clearSeconds") id . parseTime defaultTimeLocale "%H:%M:%S" . formatTime defaultTimeLocale "%H:%M:00"

modifyFailureProp :: String -> String -> Bool
modifyFailureProp orig added =
    result == Error (added ++ orig)
  where
    parser = const $ modifyFailure (added ++) $ fail orig
    result :: Result ()
    result = parse parser ()

main :: IO ()
main = do
    comparisonTest <- encoderComparisonTests
    defaultMain (comparisonTest : tests)

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
   testGroup "camelCase" [
      testCase "camelTo" $ roundTripCamel "aName"
    , testCase "camelTo" $ roundTripCamel "another"
    , testCase "camelTo" $ roundTripCamel "someOtherName"
    ],
  testGroup "roundTrip" [
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
    , testProperty "ZonedTime" $ zonedTimeToJSON
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
