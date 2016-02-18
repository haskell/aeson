{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, DataKinds, MultiParamTypeClasses #-}

module Properties (
    tests
  , is2ElemArray
  , isObjectWithSingleField
  , isString
  , isTaggedObject
  , isTaggedObjectValue
  , sameAs
  , toParseJSON
  ) where

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode (encode)
import Data.Aeson.Internal (IResult(..), formatError, ifromJSON, iparse)
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.ByteString.Builder (toLazyByteString)
import Data.Hashable (Hashable(..))
import Data.Int (Int8)
import Data.Time (Day, LocalTime, NominalDiffTime, TimeOfDay, UTCTime,
                  ZonedTime)
import Data.Version (Version)
import Encoders
import Instances ()
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property, (===), (.&&.), counterexample)
import Types
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V

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
    case iparse parsejson . tojson $ x of
      IError path msg -> failure "parse" (formatError path msg) x
      ISuccess x'     -> x === x'

roundTripEnc :: (FromJSON a, ToJSON a, Show a) =>
             (a -> a -> Property) -> a -> a -> Property
roundTripEnc eq _ i =
    case fmap ifromJSON . L.parse value . encode $ i of
      L.Done _ (ISuccess v)      -> v `eq` i
      L.Done _ (IError path err) -> failure "fromJSON" (formatError path err) i
      L.Fail _ _ err             -> failure "parse" err i

roundTripNoEnc :: (FromJSON a, ToJSON a, Show a) =>
             (a -> a -> Property) -> a -> a -> Property
roundTripNoEnc eq _ i =
    case ifromJSON . toJSON $ i of
      (ISuccess v)      -> v `eq` i
      (IError path err) -> failure "fromJSON" (formatError path err) i

roundTripEq :: (Eq a, FromJSON a, ToJSON a, Show a) => a -> a -> Property
roundTripEq x y = roundTripEnc (===) x y .&&. roundTripNoEnc (===) x y

infix 4 ==~
(==~) :: (ApproxEq a, Show a) => a -> a -> Property
x ==~ y =
  counterexample (show x ++ " /= " ++ show y) (x =~ y)

roundTripApproxEq :: (ApproxEq a, FromJSON a, ToJSON a, Show a) => a -> a -> Property
roundTripApproxEq x y = roundTripEnc (==~) x y .&&. roundTripNoEnc (==~) x y

toFromJSON :: (Arbitrary a, Eq a, FromJSON a, ToJSON a, Show a) => a -> Property
toFromJSON x = case ifromJSON (toJSON x) of
                IError path err -> failure "fromJSON" (formatError path err) x
                ISuccess x'     -> x === x'

modifyFailureProp :: String -> String -> Bool
modifyFailureProp orig added =
    result == Error (added ++ orig)
  where
    parser = const $ modifyFailure (added ++) $ fail orig
    result :: Result ()
    result = parse parser ()

-- | Perform a structural comparison of the results of two encoding
-- methods. Compares decoded values to account for HashMap-driven
-- variation in JSON object key ordering.
sameAs :: (a -> Value) -> (a -> Encoding) -> a -> Property
sameAs toVal toEnc v =
  eitherDecode (toLazyByteString (fromEncoding (toEnc v))) === Right (toVal v)

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

--------------------------------------------------------------------------------
-- Map key variants
--------------------------------------------------------------------------------

newtype T1 = T1 T.Text
  deriving (Show, Eq, Ord)

instance Hashable T1 where
  hashWithSalt salt (T1 t) = hashWithSalt salt t
instance Arbitrary T1 where
  arbitrary = fmap T1 arbitrary
instance FromJSONKey T1 'JSONKeyIdentity where
  fromJSONKey _ = T1
instance ToJSONKey T1 'JSONKeyIdentity where
  toJSONKey _ (T1 t) = t

newtype T2 = T2 T.Text
  deriving (Show, Eq, Ord)

instance Hashable T2 where
  hashWithSalt salt (T2 t) = hashWithSalt salt t
instance Arbitrary T2 where
  arbitrary = fmap T2 arbitrary
instance FromJSONKey T2 'JSONKeyCoerce where
  fromJSONKey _ = ()
instance ToJSONKey T2 'JSONKeyCoerce where
  toJSONKey _ = ()

newtype T3 = T3 T.Text
  deriving (Show, Eq, Ord)

instance Hashable T3 where
  hashWithSalt salt (T3 t) = hashWithSalt salt t
instance Arbitrary T3 where
  arbitrary = fmap T3 arbitrary
instance FromJSONKey T3 'JSONKeyTextParser where
  fromJSONKey _ = return . T3
instance ToJSONKey T3 'JSONKeyTextParser where
  toJSONKey _ (T3 t) = t

--------------------------------------------------------------------------------

tests :: Test
tests = testGroup "properties" [
  testGroup "encode" [
      testProperty "encodeDouble" encodeDouble
    , testProperty "encodeInteger" encodeInteger
    ]
  , testGroup "roundTrip" [
      testProperty "Bool" $ roundTripEq True
    , testProperty "Double" $ roundTripEq (1 :: Approx Double)
    , testProperty "Int" $ roundTripEq (1 :: Int)
    , testProperty "Integer" $ roundTripEq (1 :: Integer)
    , testProperty "String" $ roundTripEq ("" :: String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Foo" $ roundTripEq (undefined :: Foo)
    , testProperty "Day" $ roundTripEq (undefined :: Day)
    , testProperty "DotNetTime" $ roundTripApproxEq (undefined :: DotNetTime)
    , testProperty "LocalTime" $ roundTripEq (undefined :: LocalTime)
    , testProperty "TimeOfDay" $ roundTripEq (undefined :: TimeOfDay)
    , testProperty "UTCTime" $ roundTripEq (undefined :: UTCTime)
    , testProperty "ZonedTime" $ roundTripEq (undefined :: ZonedTime)
    , testProperty "NominalDiffTime" $ roundTripEq (undefined :: NominalDiffTime)
    , testProperty "Version" $ roundTripEq (undefined :: Version)
    , testGroup "ghcGenerics" [
        testProperty "OneConstructor" $ roundTripEq OneConstructor
      , testProperty "Product2" $ roundTripEq (undefined :: Product2 Int Bool)
      , testProperty "Product6" $ roundTripEq (undefined :: P6)
      , testProperty "Sum4" $ roundTripEq (undefined :: S4)
      ]
    ]
    , testGroup "Map" [
        testProperty "T1" $ roundTripEq (undefined :: Map.Map T1 Int)
      , testProperty "T2" $ roundTripEq (undefined :: Map.Map T2 Int)
      , testProperty "T3" $ roundTripEq (undefined :: Map.Map T3 Int)
      , testProperty "Text" $ roundTripEq (undefined :: Map.Map T.Text Int)
      , testProperty "String" $ roundTripEq (undefined :: Map.Map String Int)
      , testProperty "(Char, Char)" $ roundTripEq (undefined :: Map.Map (Char, Char) Int)
      ]
    , testGroup "HashMap" [
        testProperty "T1" $ roundTripEq (undefined :: H.HashMap T1 Int)
      , testProperty "T2" $ roundTripEq (undefined :: H.HashMap T2 Int)
      , testProperty "T3" $ roundTripEq (undefined :: H.HashMap T3 Int)
      , testProperty "Text" $ roundTripEq (undefined :: H.HashMap T.Text Int)
      , testProperty "String" $ roundTripEq (undefined :: H.HashMap String Int)
      , testProperty "(Char, Char)" $ roundTripEq (undefined :: H.HashMap (Char, Char) Int)
      ]
  , testGroup "toFromJSON" [
      testProperty "Integer" (toFromJSON :: Integer -> Property)
    , testProperty "Double" (toFromJSON :: Double -> Property)
    , testProperty "Maybe Integer" (toFromJSON :: Maybe Integer -> Property)
    , testProperty "Either Integer Double" (toFromJSON :: Either Integer Double -> Property)
    , testProperty "Either Integer Integer" (toFromJSON :: Either Integer Integer -> Property)
    ]
  , testGroup "failure messages" [
      testProperty "modify failure" modifyFailureProp
    ]
  , testGroup "template-haskell" [
      testGroup "toJSON" [
        testGroup "Nullary" [
            testProperty "string" (isString . thNullaryToJSONString)
          , testProperty "2ElemArray" (is2ElemArray . thNullaryToJSON2ElemArray)
          , testProperty "TaggedObject" (isTaggedObjectValue . thNullaryToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thNullaryToJSONObjectWithSingleField)

          , testGroup "roundTrip" [
              testProperty "string" (toParseJSON thNullaryParseJSONString thNullaryToJSONString)
            , testProperty "2ElemArray" (toParseJSON thNullaryParseJSON2ElemArray thNullaryToJSON2ElemArray)
            , testProperty "TaggedObject" (toParseJSON thNullaryParseJSONTaggedObject thNullaryToJSONTaggedObject)
            , testProperty "ObjectWithSingleField" (toParseJSON thNullaryParseJSONObjectWithSingleField thNullaryToJSONObjectWithSingleField)
            ]
        ]
      , testGroup "SomeType" [
          testProperty "2ElemArray" (is2ElemArray . thSomeTypeToJSON2ElemArray)
        , testProperty "TaggedObject" (isTaggedObject . thSomeTypeToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thSomeTypeToJSONObjectWithSingleField)
        , testGroup "roundTrip" [
            testProperty "2ElemArray" (toParseJSON thSomeTypeParseJSON2ElemArray thSomeTypeToJSON2ElemArray)
          , testProperty "TaggedObject" (toParseJSON thSomeTypeParseJSONTaggedObject thSomeTypeToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (toParseJSON thSomeTypeParseJSONObjectWithSingleField thSomeTypeToJSONObjectWithSingleField)
          ]
       , testGroup "Approx" [
            testProperty "string"                (isString                . thApproxToJSONUnwrap)
          , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thApproxToJSONDefault)
          , testGroup "roundTrip" [
                testProperty "string"                (toParseJSON thApproxParseJSONUnwrap  thApproxToJSONUnwrap)
              , testProperty "ObjectWithSingleField" (toParseJSON thApproxParseJSONDefault thApproxToJSONDefault)
            ]
          ]
        , testGroup "GADT" [
            testProperty "string"                (isString                . thGADTToJSONUnwrap)
          , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thGADTToJSONDefault)
          , testGroup "roundTrip" [
                testProperty "string"                (toParseJSON thGADTParseJSONUnwrap  thGADTToJSONUnwrap)
              , testProperty "ObjectWithSingleField" (toParseJSON thGADTParseJSONDefault thGADTToJSONDefault)
            ]
          ]
        ]
      ]
    , testGroup "toEncoding" [
        testProperty "NullaryString" $
        thNullaryToJSONString `sameAs` thNullaryToEncodingString
      , testProperty "Nullary2ElemArray" $
        thNullaryToJSON2ElemArray `sameAs` thNullaryToEncoding2ElemArray
      , testProperty "NullaryTaggedObject" $
        thNullaryToJSONTaggedObject `sameAs` thNullaryToEncodingTaggedObject
      , testProperty "NullaryObjectWithSingleField" $
        thNullaryToJSONObjectWithSingleField `sameAs`
        thNullaryToEncodingObjectWithSingleField
      , testProperty "ApproxUnwrap" $
        thApproxToJSONUnwrap `sameAs` thApproxToEncodingUnwrap
      , testProperty "ApproxDefault" $
        thApproxToJSONDefault `sameAs` thApproxToEncodingDefault
      , testProperty "SomeType2ElemArray" $
        thSomeTypeToJSON2ElemArray `sameAs` thSomeTypeToEncoding2ElemArray
      , testProperty "SomeTypeTaggedObject" $
        thSomeTypeToJSONTaggedObject `sameAs` thSomeTypeToEncodingTaggedObject
      , testProperty "SomeTypeObjectWithSingleField" $
        thSomeTypeToJSONObjectWithSingleField `sameAs`
        thSomeTypeToEncodingObjectWithSingleField
      ]
    ]
  ]
