{-# LANGUAGE CPP, DeriveDataTypeable, OverloadedStrings, RecordWildCards,
    ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifdef GENERICS
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
#endif

import Control.Monad
import Control.Applicative
import Data.Aeson.Encode
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Attoparsec.Number
import Data.Data (Typeable, Data)
import Data.Function (on)
import Data.Text (Text)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), choose, oneof, elements, Gen)
import qualified Data.Vector as V
import qualified Data.Aeson.Generic as G
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as H
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime(..), picosecondsToDiffTime)
import Data.Time (ZonedTime(..), LocalTime(..), TimeZone(..),
                  hoursToTimeZone, Day(..), TimeOfDay(..))
import Options

#ifdef GENERICS
import GHC.Generics
#endif

encodeDouble :: Double -> Double -> Bool
encodeDouble num denom
    | isInfinite d || isNaN d = encode (Number (D d)) == "null"
    | otherwise               = (read . L.unpack . encode . Number . D) d == d
  where d = num / denom

encodeInteger :: Integer -> Bool
encodeInteger i = encode (Number (I i)) == L.pack (show i)

roundTrip :: (FromJSON a, ToJSON a) => (a -> a -> Bool) -> a -> a -> Bool
roundTrip eq _ i =
    case fmap fromJSON . L.parse value . encode . toJSON $ i of
      L.Done _ (Success v) -> v `eq` i
      _                    -> False

roundTripEq :: (Eq a, FromJSON a, ToJSON a) => a -> a -> Bool
roundTripEq x y = roundTrip (==) x y

genericTo :: (Data a, ToJSON a) => a -> a -> Bool
genericTo _ v = G.toJSON v == toJSON v

genericFrom :: (Eq a, Data a, ToJSON a) => a -> a -> Bool
genericFrom _ v = G.fromJSON (toJSON v) == Success v

approxEq :: (Fractional a, Ord a) => a -> a -> Bool
approxEq = approxEqWith 1e-15 1e-15

approxEqWith :: (Fractional a, Ord a) => a -> a -> a -> a -> Bool
approxEqWith maxAbsoluteError maxRelativeError a b =
    a == b || d < maxAbsoluteError ||
    d / max (abs b) (abs a) <= maxRelativeError
  where d = abs (a - b)

-- Compare equality to within a millisecond, allowing for rounding
-- error (ECMA 262 requires milliseconds to rounded to zero, not
-- rounded to nearest).
approxEqUTC :: UTCTime -> UTCTime -> Bool
approxEqUTC a b = ((==) `on` utctDay) a b &&
                  (approxEqWith 1 1 `on` ((* 1e3) . utctDayTime)) a b

approxEqNet :: DotNetTime -> DotNetTime -> Bool
approxEqNet (DotNetTime a) (DotNetTime b) = approxEqUTC a b

toFromJSON :: (Arbitrary a, Eq a, FromJSON a, ToJSON a) => a -> Bool
toFromJSON x = case fromJSON . toJSON $ x of
                Error _ -> False
                Success x' -> x == x'

genericToFromJSON :: (Arbitrary a, Eq a, Data a) => a -> Bool
genericToFromJSON x = case G.fromJSON . G.toJSON $ x of
                Error _ -> False
                Success x' -> x == x'

toParseJSON :: (Arbitrary a, Eq a) => (Value -> Parser a) -> (a -> Value) -> a -> Bool
toParseJSON parsejson tojson x = case parse parsejson . tojson $ x of
                Error _ -> False
                Success x' -> x == x'

regress_gh72 :: [(String, Maybe String)] -> Bool
regress_gh72 ys = G.decode (G.encode m) == Just m
    where m = Map.fromList ys

modifyFailureProp :: String -> String -> Bool
modifyFailureProp orig added =
    result == Error (added ++ orig)
  where
    parser = const $ modifyFailure (added ++) $ fail orig
    result :: Result ()
    result = parse parser ()

data Foo = Foo {
      fooInt :: Int
    , fooDouble :: Double
    , fooTuple :: (String, Text, Int)
    -- This definition causes an infinite loop in genericTo and genericFrom!
    -- , fooMap :: Map.Map String Foo
    , fooMap :: Map.Map String (Text,Int)
    } deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- Nullary
--------------------------------------------------------------------------------

data Nullary = C1 | C2 | C3 deriving (Eq, Show)

instance Arbitrary Nullary where
    arbitrary = elements [C1, C2, C3]

thNullaryToJSONString :: Nullary -> Value
thNullaryToJSONString = $(mkToJSON optsDefault ''Nullary)

thNullaryParseJSONString :: Value -> Parser Nullary
thNullaryParseJSONString = $(mkParseJSON optsDefault ''Nullary)


thNullaryToJSON2ElemArray :: Nullary -> Value
thNullaryToJSON2ElemArray = $(mkToJSON opts2ElemArray ''Nullary)

thNullaryParseJSON2ElemArray :: Value -> Parser Nullary
thNullaryParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''Nullary)


thNullaryToJSONObjectWithType :: Nullary -> Value
thNullaryToJSONObjectWithType = $(mkToJSON optsObjectWithType ''Nullary)

thNullaryParseJSONObjectWithType :: Value -> Parser Nullary
thNullaryParseJSONObjectWithType = $(mkParseJSON optsObjectWithType ''Nullary)


thNullaryToJSONObjectWithSingleField :: Nullary -> Value
thNullaryToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''Nullary)

thNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
thNullaryParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''Nullary)

#ifdef GENERICS
deriving instance Generic Nullary

gNullaryToJSONString :: Nullary -> Value
gNullaryToJSONString = gToJSON optsDefault . from

gNullaryParseJSONString :: Value -> Parser Nullary
gNullaryParseJSONString = fmap to . gParseJSON optsDefault


gNullaryToJSON2ElemArray :: Nullary -> Value
gNullaryToJSON2ElemArray = gToJSON opts2ElemArray . from

gNullaryParseJSON2ElemArray :: Value -> Parser Nullary
gNullaryParseJSON2ElemArray = fmap to . gParseJSON opts2ElemArray


gNullaryToJSONObjectWithType :: Nullary -> Value
gNullaryToJSONObjectWithType = gToJSON optsObjectWithType . from

gNullaryParseJSONObjectWithType :: Value -> Parser Nullary
gNullaryParseJSONObjectWithType = fmap to . gParseJSON optsObjectWithType


gNullaryToJSONObjectWithSingleField :: Nullary -> Value
gNullaryToJSONObjectWithSingleField = gToJSON optsObjectWithSingleField . from

gNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
gNullaryParseJSONObjectWithSingleField = fmap to . gParseJSON optsObjectWithSingleField
#endif

--------------------------------------------------------------------------------
-- SomeType
--------------------------------------------------------------------------------

data SomeType a = Nullary
                | Unary Int
                | Product String (Maybe Char) a
                | Record { testOne   :: Double
                         , testTwo   :: Maybe Bool
                         , testThree :: Maybe a
                         } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (SomeType a) where
    arbitrary = oneof [ pure Nullary
                      , Unary   <$> arbitrary
                      , Product <$> arbitrary <*> arbitrary <*> arbitrary
                      , Record  <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

type SomeTypeToJSON = SomeType Int -> Value

thSomeTypeToJSON2ElemArray :: ToJSON a => SomeType a -> Value
thSomeTypeToJSON2ElemArray = $(mkToJSON opts2ElemArray ''SomeType)

thSomeTypeParseJSON2ElemArray :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''SomeType)


thSomeTypeToJSONObjectWithType :: ToJSON a => SomeType a -> Value
thSomeTypeToJSONObjectWithType = $(mkToJSON optsObjectWithType ''SomeType)

thSomeTypeParseJSONObjectWithType :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSONObjectWithType = $(mkParseJSON optsObjectWithType ''SomeType)


thSomeTypeToJSONObjectWithSingleField :: ToJSON a => SomeType a -> Value
thSomeTypeToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''SomeType)

thSomeTypeParseJSONObjectWithSingleField :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''SomeType)

#ifdef GENERICS
deriving instance Generic (SomeType a)

gSomeTypeToJSON2ElemArray :: ToJSON a => SomeType a -> Value
gSomeTypeToJSON2ElemArray = gToJSON opts2ElemArray . from

gSomeTypeParseJSON2ElemArray :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSON2ElemArray = fmap to . gParseJSON opts2ElemArray


gSomeTypeToJSONObjectWithType :: ToJSON a => SomeType a -> Value
gSomeTypeToJSONObjectWithType = gToJSON optsObjectWithType . from

gSomeTypeParseJSONObjectWithType :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSONObjectWithType = fmap to . gParseJSON optsObjectWithType


gSomeTypeToJSONObjectWithSingleField :: ToJSON a => SomeType a -> Value
gSomeTypeToJSONObjectWithSingleField = gToJSON optsObjectWithSingleField . from

gSomeTypeParseJSONObjectWithSingleField :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSONObjectWithSingleField = fmap to . gParseJSON optsObjectWithSingleField
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

isObjectWithTypeValue :: Value -> Bool
isObjectWithTypeValue (Object obj) = "type"  `H.member` obj &&
                                     "value" `H.member` obj
isObjectWithTypeValue _            = False

isObjectWithType :: Value -> Bool
isObjectWithType (Object obj) = "type"  `H.member` obj
isObjectWithType _            = False

isObjectWithSingleField :: Value -> Bool
isObjectWithSingleField (Object obj) = H.size obj == 1
isObjectWithSingleField _            = False

--------------------------------------------------------------------------------

instance Eq Foo where
    a == b = fooInt a == fooInt b &&
             fooDouble a `approxEq` fooDouble b &&
             fooTuple a == fooTuple b

instance ToJSON Foo where
    toJSON Foo{..} = object [ "fooInt" .= fooInt
                            , "fooDouble" .= fooDouble
                            , "fooTuple" .= fooTuple
                            , "fooMap" .= fooMap
                            ]

instance FromJSON Foo where
    parseJSON (Object v) = Foo <$>
                           v .: "fooInt" <*>
                           v .: "fooDouble" <*>
                           v .: "fooTuple" <*>
                           v .: "fooMap"
    parseJSON _ = empty

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> arbitrary

instance Arbitrary Foo where
    arbitrary = liftM4 Foo arbitrary arbitrary arbitrary arbitrary

instance Arbitrary LocalTime where
    arbitrary = return $ LocalTime (ModifiedJulianDay 1) (TimeOfDay 1 2 3)

instance Arbitrary TimeZone where
    arbitrary = do
      offset <- choose (0,2) :: Gen Int
      return $ hoursToTimeZone offset

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay `liftM` arbitrary

instance Arbitrary DiffTime where
    arbitrary = picosecondsToDiffTime `liftM` choose (0, 86400000000000000)

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary

instance Arbitrary DotNetTime where
    arbitrary = DotNetTime `liftM` arbitrary

instance Arbitrary ZonedTime where
    arbitrary = liftM2 ZonedTime arbitrary arbitrary

data UFoo = UFoo {
      _UFooInt :: Int
    , uFooInt :: Int
    } deriving (Show, Eq, Data, Typeable)

instance Arbitrary UFoo where
    arbitrary = UFoo <$> arbitrary <*> arbitrary
        where _ = uFooInt

main :: IO ()
main = defaultMain tests

deriving instance Eq ZonedTime

tests :: [Test]
tests = [
  testGroup "regression" [
      testProperty "gh-72" regress_gh72
  ],
  testGroup "encode" [
      testProperty "encodeDouble" encodeDouble
    , testProperty "encodeInteger" encodeInteger
    ],
  testGroup "genericFrom" [
      testProperty "Bool" $ genericFrom True
    , testProperty "Double" $ genericFrom (1::Double)
    , testProperty "Int" $ genericFrom (1::Int)
    , testProperty "Foo" $ genericFrom (undefined::Foo)
    , testProperty "Maybe" $ genericFrom (Just 1 :: Maybe Int)
    ],
  testGroup "genericTo" [
      testProperty "Bool" $ genericTo True
    , testProperty "Double" $ genericTo (1::Double)
    , testProperty "Int" $ genericTo (1::Int)
    , testProperty "Foo" $ genericTo (undefined::Foo)
    , testProperty "Maybe" $ genericTo (Just 1 :: Maybe Int)
    ],
  testGroup "roundTrip" [
      testProperty "Bool" $ roundTripEq True
    , testProperty "Double" $ roundTrip approxEq (1::Double)
    , testProperty "Int" $ roundTripEq (1::Int)
    , testProperty "Integer" $ roundTripEq (1::Integer)
    , testProperty "String" $ roundTripEq (""::String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Foo" $ roundTripEq (undefined::Foo)
    , testProperty "DotNetTime" $ roundTrip approxEqNet undefined
    , testProperty "UTCTime" $ roundTrip approxEqUTC undefined
    , testProperty "ZonedTime" $ roundTripEq (undefined::ZonedTime)
    ],
  testGroup "toFromJSON" [
      testProperty "Integer" (toFromJSON :: Integer -> Bool)
    , testProperty "Double" (toFromJSON :: Double -> Bool)
    , testProperty "Maybe Integer" (toFromJSON :: Maybe Integer -> Bool)
    , testProperty "Either Integer Double" (toFromJSON :: Either Integer Double -> Bool)
    , testProperty "Either Integer Integer" (toFromJSON :: Either Integer Integer -> Bool)
    ],
  testGroup "genericToFromJSON" [
      testProperty "_UFoo" (genericToFromJSON :: UFoo -> Bool)
    ],
  testGroup "failure messages" [
      testProperty "modify failure" modifyFailureProp
    ],
  testGroup "template-haskell" [
    testGroup "Nullary" [
        testProperty "string"                (isString                . thNullaryToJSONString)
      , testProperty "2ElemArray"            (is2ElemArray            . thNullaryToJSON2ElemArray)
      , testProperty "ObjectWithType"        (isObjectWithTypeValue   . thNullaryToJSONObjectWithType)
      , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thNullaryToJSONObjectWithSingleField)

      , testGroup "roundTrip" [
            testProperty "string"                (toParseJSON thNullaryParseJSONString                thNullaryToJSONString)
          , testProperty "2ElemArray"            (toParseJSON thNullaryParseJSON2ElemArray            thNullaryToJSON2ElemArray)
          , testProperty "ObjectWithType"        (toParseJSON thNullaryParseJSONObjectWithType        thNullaryToJSONObjectWithType)
          , testProperty "ObjectWithSingleField" (toParseJSON thNullaryParseJSONObjectWithSingleField thNullaryToJSONObjectWithSingleField)
        ]
      ]
   , testGroup "SomeType" [
        testProperty "2ElemArray"            (is2ElemArray            . (thSomeTypeToJSON2ElemArray            :: SomeTypeToJSON))
      , testProperty "ObjectWithType"        (isObjectWithType        . (thSomeTypeToJSONObjectWithType        :: SomeTypeToJSON))
      , testProperty "ObjectWithSingleField" (isObjectWithSingleField . (thSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))

      , testGroup "roundTrip" [
            testProperty "2ElemArray"            (toParseJSON thSomeTypeParseJSON2ElemArray            (thSomeTypeToJSON2ElemArray            :: SomeTypeToJSON))
          , testProperty "ObjectWithType"        (toParseJSON thSomeTypeParseJSONObjectWithType        (thSomeTypeToJSONObjectWithType        :: SomeTypeToJSON))
          , testProperty "ObjectWithSingleField" (toParseJSON thSomeTypeParseJSONObjectWithSingleField (thSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))
        ]
    ]
    ]
#ifdef GENERICS
  , testGroup "GHC-generics" [
      testGroup "Nullary" [
          testProperty "string"                (isString                . gNullaryToJSONString)
        , testProperty "2ElemArray"            (is2ElemArray            . gNullaryToJSON2ElemArray)
        , testProperty "ObjectWithType"        (isObjectWithTypeValue   . gNullaryToJSONObjectWithType)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . gNullaryToJSONObjectWithSingleField)

        , testGroup "eq" [
              testProperty "string"                (\n -> gNullaryToJSONString                n == thNullaryToJSONString                n)
            , testProperty "2ElemArray"            (\n -> gNullaryToJSON2ElemArray            n == thNullaryToJSON2ElemArray            n)
            , testProperty "ObjectWithType"        (\n -> gNullaryToJSONObjectWithType        n == thNullaryToJSONObjectWithType        n)
            , testProperty "ObjectWithSingleField" (\n -> gNullaryToJSONObjectWithSingleField n == thNullaryToJSONObjectWithSingleField n)
          ]
        , testGroup "roundTrip" [
            testProperty "string"                (toParseJSON gNullaryParseJSONString                gNullaryToJSONString)
          , testProperty "2ElemArray"            (toParseJSON gNullaryParseJSON2ElemArray            gNullaryToJSON2ElemArray)
          , testProperty "ObjectWithType"        (toParseJSON gNullaryParseJSONObjectWithType        gNullaryToJSONObjectWithType)
          , testProperty "ObjectWithSingleField" (toParseJSON gNullaryParseJSONObjectWithSingleField gNullaryToJSONObjectWithSingleField)
          ]
        ]
    , testGroup "SomeType" [
          testProperty "2ElemArray"            (is2ElemArray            . (gSomeTypeToJSON2ElemArray            :: SomeTypeToJSON))
        , testProperty "ObjectWithType"        (isObjectWithType        . (gSomeTypeToJSONObjectWithType        :: SomeTypeToJSON))
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . (gSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))

        , testGroup "eq" [
              testProperty "2ElemArray"            (\n -> (gSomeTypeToJSON2ElemArray            :: SomeTypeToJSON) n == thSomeTypeToJSON2ElemArray            n)
            , testProperty "ObjectWithType"        (\n -> (gSomeTypeToJSONObjectWithType        :: SomeTypeToJSON) n == thSomeTypeToJSONObjectWithType        n)
            , testProperty "ObjectWithSingleField" (\n -> (gSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON) n == thSomeTypeToJSONObjectWithSingleField n)
          ]
        , testGroup "roundTrip" [
            testProperty "2ElemArray"            (toParseJSON gSomeTypeParseJSON2ElemArray            (gSomeTypeToJSON2ElemArray            :: SomeTypeToJSON))
          , testProperty "ObjectWithType"        (toParseJSON gSomeTypeParseJSONObjectWithType        (gSomeTypeToJSONObjectWithType        :: SomeTypeToJSON))
          , testProperty "ObjectWithSingleField" (toParseJSON gSomeTypeParseJSONObjectWithSingleField (gSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))
          ]
        ]
    ]
#endif
  ]
