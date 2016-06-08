{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}

module Properties where

import Prelude ()
import Prelude.Compat

import Control.Applicative (Const)
import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Internal (IResult(..), formatError, ifromJSON, iparse)
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.ByteString.Builder (toLazyByteString)
import Data.Int (Int8)
import Data.Sequence (Seq)
import Data.DList (DList)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Ratio (Ratio)
import Data.List.NonEmpty (NonEmpty)
import Data.Time (Day, LocalTime, NominalDiffTime, TimeOfDay, UTCTime,
                  ZonedTime)
import Data.Proxy (Proxy)
import Data.Tagged (Tagged)
import Data.Version (Version)
import Encoders
import Instances ()
import Numeric.Natural (Natural)
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

-- We test keys by encoding HashMap and Map with it
roundTripKey
    :: (Ord a, Hashable a, FromJSONKey a, ToJSONKey a, Show a)
    => a -> HashMap a Int -> Map a Int -> Property
roundTripKey _ h m = roundTripEq h h .&&. roundTripEq m m

infix 4 ==~
(==~) :: (ApproxEq a, Show a) => a -> a -> Property
x ==~ y =
  counterexample (show x ++ " /= " ++ show y) (x =~ y)

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

isNullaryTaggedObject :: Value -> Bool
isNullaryTaggedObject obj = isTaggedObject obj && isObjectWithSingleField obj

isTaggedObject :: Value -> Bool
isTaggedObject (Object obj) = "tag" `H.member` obj
isTaggedObject _            = False

isObjectWithSingleField :: Value -> Bool
isObjectWithSingleField (Object obj) = H.size obj == 1
isObjectWithSingleField _            = False

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
    , testProperty "NonEmpty Char" $ roundTripEq (undefined :: NonEmpty Char)
    , testProperty "Integer" $ roundTripEq (1 :: Integer)
    , testProperty "String" $ roundTripEq ("" :: String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Foo" $ roundTripEq (undefined :: Foo)
    , testProperty "Day" $ roundTripEq (undefined :: Day)
    , testProperty "DotNetTime" $ roundTripEq (undefined :: Approx DotNetTime)
    , testProperty "LocalTime" $ roundTripEq (undefined :: LocalTime)
    , testProperty "TimeOfDay" $ roundTripEq (undefined :: TimeOfDay)
    , testProperty "UTCTime" $ roundTripEq (undefined :: UTCTime)
    , testProperty "ZonedTime" $ roundTripEq (undefined :: ZonedTime)
    , testProperty "NominalDiffTime" $ roundTripEq (undefined :: NominalDiffTime)
    , testProperty "Version" $ roundTripEq (undefined :: Version)
    , testProperty "Natural" $ roundTripEq (undefined :: Natural)
    , testProperty "Proxy" $ roundTripEq (undefined :: Proxy Int)
    , testProperty "Tagged" $ roundTripEq (undefined :: Tagged Int Char)
    , testProperty "Const" $ roundTripEq (undefined :: Const Int Char)
    , testProperty "DList" $ roundTripEq (undefined :: DList Int)
    , testProperty "Seq" $ roundTripEq (undefined :: Seq Int)
    , testProperty "Rational" $ roundTripEq (undefined :: Rational)
    , testProperty "Ratio Int" $ roundTripEq (undefined :: Ratio Int)
    , testGroup "ghcGenerics" [
        testProperty "OneConstructor" $ roundTripEq OneConstructor
      , testProperty "Product2" $ roundTripEq (undefined :: Product2 Int Bool)
      , testProperty "Product6" $ roundTripEq (undefined :: P6)
      , testProperty "Sum4" $ roundTripEq (undefined :: S4)
      ]
    ]
  , testGroup "roundTrip Key"
    [ testProperty "Bool" $ roundTripKey True
    , testProperty "Text" $ roundTripKey (undefined :: T.Text)
    , testProperty "String" $ roundTripKey (undefined :: String)
    , testProperty "Int" $ roundTripKey (undefined :: Int)
    , testProperty "[Text]" $ roundTripKey (undefined :: [T.Text])
    , testProperty "(Int,Char)" $ roundTripKey (undefined :: (Int,Char))
    , testProperty "Integer" $ roundTripKey (undefined :: Integer)
    , testProperty "Natural" $ roundTripKey (undefined :: Natural)
    , testProperty "Float" $ roundTripKey (undefined :: Float)
    , testProperty "Double" $ roundTripKey (undefined :: Double)
#if MIN_VERSION_base(4,7,0)
    , testProperty "Day" $ roundTripKey (undefined :: Day)
    , testProperty "LocalTime" $ roundTripKey (undefined :: LocalTime)
    , testProperty "TimeOfDay" $ roundTripKey (undefined :: TimeOfDay)
    , testProperty "UTCTime" $ roundTripKey (undefined :: UTCTime)
#endif
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
  , testGroup "generic" [
      testGroup "toJSON" [
        testGroup "Nullary" [
            testProperty "string" (isString . gNullaryToJSONString)
          , testProperty "2ElemArray" (is2ElemArray . gNullaryToJSON2ElemArray)
          , testProperty "TaggedObject" (isNullaryTaggedObject . gNullaryToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (isObjectWithSingleField . gNullaryToJSONObjectWithSingleField)
          , testGroup "roundTrip" [
              testProperty "string" (toParseJSON gNullaryParseJSONString gNullaryToJSONString)
            , testProperty "2ElemArray" (toParseJSON gNullaryParseJSON2ElemArray gNullaryToJSON2ElemArray)
            , testProperty "TaggedObject" (toParseJSON gNullaryParseJSONTaggedObject gNullaryToJSONTaggedObject)
            , testProperty "ObjectWithSingleField" (toParseJSON gNullaryParseJSONObjectWithSingleField gNullaryToJSONObjectWithSingleField)
            ]
        ]
      , testGroup "SomeType" [
          testProperty "2ElemArray" (is2ElemArray . gSomeTypeToJSON2ElemArray)
        , testProperty "TaggedObject" (isTaggedObject . gSomeTypeToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . gSomeTypeToJSONObjectWithSingleField)
        , testGroup "roundTrip" [
            testProperty "2ElemArray" (toParseJSON gSomeTypeParseJSON2ElemArray gSomeTypeToJSON2ElemArray)
          , testProperty "TaggedObject" (toParseJSON gSomeTypeParseJSONTaggedObject gSomeTypeToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (toParseJSON gSomeTypeParseJSONObjectWithSingleField gSomeTypeToJSONObjectWithSingleField)
          ]
        ]
      ]
    , testGroup "toEncoding" [
        testProperty "NullaryString" $
        gNullaryToJSONString `sameAs` gNullaryToEncodingString
      , testProperty "Nullary2ElemArray" $
        gNullaryToJSON2ElemArray `sameAs` gNullaryToEncoding2ElemArray
      , testProperty "NullaryTaggedObject" $
        gNullaryToJSONTaggedObject `sameAs` gNullaryToEncodingTaggedObject
      , testProperty "NullaryObjectWithSingleField" $
        gNullaryToJSONObjectWithSingleField `sameAs`
        gNullaryToEncodingObjectWithSingleField
      -- , testProperty "ApproxUnwrap" $
      --   gApproxToJSONUnwrap `sameAs` gApproxToEncodingUnwrap
      , testProperty "ApproxDefault" $
        gApproxToJSONDefault `sameAs` gApproxToEncodingDefault
      , testProperty "SomeType2ElemArray" $
        gSomeTypeToJSON2ElemArray `sameAs` gSomeTypeToEncoding2ElemArray
      , testProperty "SomeTypeTaggedObject" $
        gSomeTypeToJSONTaggedObject `sameAs` gSomeTypeToEncodingTaggedObject
      , testProperty "SomeTypeObjectWithSingleField" $
        gSomeTypeToJSONObjectWithSingleField `sameAs`
        gSomeTypeToEncodingObjectWithSingleField
      , testProperty "SomeTypeOmitNothingFields" $
        gSomeTypeToJSONOmitNothingFields `sameAs` gSomeTypeToEncodingOmitNothingFields
      ]
    ]
  , testGroup "template-haskell" [
      testGroup "toJSON" [
        testGroup "Nullary" [
            testProperty "string" (isString . thNullaryToJSONString)
          , testProperty "2ElemArray" (is2ElemArray . thNullaryToJSON2ElemArray)
          , testProperty "TaggedObject" (isNullaryTaggedObject . thNullaryToJSONTaggedObject)
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
