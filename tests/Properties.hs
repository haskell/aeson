{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-} -- For arbitrary Compose
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Properties (module Properties) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (Const)
import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Internal (IResult(..), formatError, ifromJSON, iparse)
import qualified Data.Aeson.Internal as I
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.DList (DList)
import Data.Functor.Compose (Compose (..))
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Int (Int8)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Proxy (Proxy)
import Data.Ratio (Ratio)
import Data.Semigroup (Option(..))
import Data.Sequence (Seq)
import Data.Tagged (Tagged)
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Version (Version)
import Encoders
import Instances ()
import Numeric.Natural (Natural)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property, Testable, (===), (.&&.), counterexample)
import Types
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V

encodeDouble :: Double -> Double -> Property
encodeDouble num denom
    | isInfinite d || isNaN d = encode d === "null"
    | otherwise               = (read . L.unpack . encode) d === d
  where d = num / denom

encodeInteger :: Integer -> Property
encodeInteger i = encode i === L.pack (show i)

toParseJSON :: (Eq a, Show a) =>
               (Value -> Parser a) -> (a -> Value) -> a -> Property
toParseJSON parsejson tojson x =
    case iparse parsejson . tojson $ x of
      IError path msg -> failure "parse" (formatError path msg) x
      ISuccess x'     -> x === x'

toParseJSON1
    :: (Eq (f Int), Show (f Int))
    => (forall a. LiftParseJSON f a)
    -> (forall a. LiftToJSON f a)
    -> f Int
    -> Property
toParseJSON1 parsejson1 tojson1 = toParseJSON parsejson tojson
  where
    parsejson = parsejson1 parseJSON (listParser parseJSON)
    tojson    = tojson1 toJSON (listValue toJSON)

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

parserThrowErrorProp :: String -> Property
parserThrowErrorProp msg =
    result === Error msg
  where
    parser = const $ parserThrowError [] msg
    result :: Result ()
    result = parse parser ()

-- | Tests (also) that we catch the JSONPath and it has elements in the right order.
parserCatchErrorProp :: [String] -> String -> Property
parserCatchErrorProp path msg =
    result === Success ([I.Key "outer", I.Key "inner"] ++ jsonPath, msg)
  where
    parser = parserCatchError outer (curry pure)

    outer = inner I.<?> I.Key "outer"
    inner = parserThrowError jsonPath msg I.<?> I.Key "inner"

    result :: Result (I.JSONPath, String)
    result = parse (const parser) ()

    jsonPath = map (I.Key . T.pack) path

-- | Perform a structural comparison of the results of two encoding
-- methods. Compares decoded values to account for HashMap-driven
-- variation in JSON object key ordering.
sameAs :: (a -> Value) -> (a -> Encoding) -> a -> Property
sameAs toVal toEnc v =
  counterexample (show s) $
    eitherDecode s === Right (toVal v)
  where
    s = encodingToLazyByteString (toEnc v)

sameAs1
    :: (forall a. LiftToJSON f a)
    -> (forall a. LiftToEncoding f a)
    -> f Int
    -> Property
sameAs1 toVal1 toEnc1 v = lhs === rhs
  where
    rhs = Right $ toVal1 toJSON (listValue toJSON) v
    lhs = eitherDecode . encodingToLazyByteString $
        toEnc1 toEncoding (listEncoding toEncoding) v

sameAs1Agree
    :: ToJSON a
    => (f a -> Encoding)
    -> (forall b. LiftToEncoding f b)
    -> f a
    -> Property
sameAs1Agree toEnc toEnc1 v = rhs === lhs
  where
    rhs = encodingToLazyByteString $ toEnc v
    lhs = encodingToLazyByteString $ toEnc1 toEncoding (listEncoding toEncoding) v

type P6 = Product6 Int Bool String (Approx Double) (Int, Approx Double) ()
type S4 = Sum4 Int8 ZonedTime T.Text (Map.Map String Int)

--------------------------------------------------------------------------------
-- Value properties
--------------------------------------------------------------------------------

-- | Add the formatted @Value@ to the printed counterexample when the property
-- fails.
checkValue :: Testable a => (Value -> a) -> Value -> Property
checkValue prop v = counterexample (L.unpack (encode v)) (prop v)

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
isNullaryTaggedObject obj = isTaggedObject' obj && isObjectWithSingleField obj

isTaggedObject :: Value -> Property
isTaggedObject = checkValue isTaggedObject'

isTaggedObject' :: Value -> Bool
isTaggedObject' (Object obj) = "tag" `H.member` obj
isTaggedObject' _            = False

isObjectWithSingleField :: Value -> Bool
isObjectWithSingleField (Object obj) = H.size obj == 1
isObjectWithSingleField _            = False

-- | is untaggedValue of EitherTextInt
isUntaggedValueETI :: Value -> Bool
isUntaggedValueETI (String s)
    | s == "nonenullary"   = True
isUntaggedValueETI (Bool _)   = True
isUntaggedValueETI (Number _) = True
isUntaggedValueETI (Array a)  = length a == 2
isUntaggedValueETI _          = False

isEmptyArray :: Value -> Property
isEmptyArray = checkValue isEmptyArray'

isEmptyArray' :: Value -> Bool
isEmptyArray' = (Array mempty ==)


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
    , testProperty "Lazy Text" $ roundTripEq LT.empty
    , testProperty "Foo" $ roundTripEq (undefined :: Foo)
    , testProperty "Day" $ roundTripEq (undefined :: Day)
    , testProperty "BCE Day" $ roundTripEq (undefined :: BCEDay)
    , testProperty "DotNetTime" $ roundTripEq (undefined :: Approx DotNetTime)
    , testProperty "LocalTime" $ roundTripEq (undefined :: LocalTime)
    , testProperty "TimeOfDay" $ roundTripEq (undefined :: TimeOfDay)
    , testProperty "UTCTime" $ roundTripEq (undefined :: UTCTime)
    , testProperty "ZonedTime" $ roundTripEq (undefined :: ZonedTime)
    , testProperty "NominalDiffTime" $ roundTripEq (undefined :: NominalDiffTime)
    , testProperty "DiffTime" $ roundTripEq (undefined :: DiffTime)
    , testProperty "Version" $ roundTripEq (undefined :: Version)
    , testProperty "Natural" $ roundTripEq (undefined :: Natural)
    , testProperty "Proxy" $ roundTripEq (undefined :: Proxy Int)
    , testProperty "Tagged" $ roundTripEq (undefined :: Tagged Int Char)
    , testProperty "Const" $ roundTripEq (undefined :: Const Int Char)
    , testProperty "DList" $ roundTripEq (undefined :: DList Int)
    , testProperty "Seq" $ roundTripEq (undefined :: Seq Int)
    , testProperty "Rational" $ roundTripEq (undefined :: Rational)
    , testProperty "Ratio Int" $ roundTripEq (undefined :: Ratio Int)
    , testProperty "UUID" $ roundTripEq UUID.nil
    , testGroup "functors"
      [ testProperty "Identity Char" $ roundTripEq (undefined :: I Int)

      , testProperty "Identity Char" $ roundTripEq (undefined :: I Char)
      , testProperty "Identity [Char]" $ roundTripEq (undefined :: I String)
      , testProperty "[Identity Char]" $ roundTripEq (undefined :: [I Char])

      , testProperty "Compose I  I  Int" $ roundTripEq (undefined :: LogScaled (Compose I  I  Int))
      , testProperty "Compose [] I  Int" $ roundTripEq (undefined :: LogScaled (Compose [] I  Int))
      , testProperty "Compose I  [] Int" $ roundTripEq (undefined :: LogScaled (Compose I  [] Int))
      , testProperty "Compose [] [] Int" $ roundTripEq (undefined :: LogScaled (Compose [] [] Int))

      , testProperty "Compose I  I  Char" $ roundTripEq (undefined :: LogScaled (Compose I  I  Char))
      , testProperty "Compose [] I  Char" $ roundTripEq (undefined :: LogScaled (Compose [] I  Char))
      , testProperty "Compose I  [] Char" $ roundTripEq (undefined :: LogScaled (Compose I  [] Char))
      , testProperty "Compose [] [] Char" $ roundTripEq (undefined :: LogScaled (Compose [] [] Char))

      , testProperty "Compose3 I  I  I  Char" $ roundTripEq (undefined :: LogScaled (Compose3 I  I  I  Char))
      , testProperty "Compose3 I  [] I  Char" $ roundTripEq (undefined :: LogScaled (Compose3 I  [] I  Char))
      , testProperty "Compose3 I  I  [] Char" $ roundTripEq (undefined :: LogScaled (Compose3 I  I  [] Char))
      , testProperty "Compose3 I  [] [] Char" $ roundTripEq (undefined :: LogScaled (Compose3 I  [] [] Char))
      , testProperty "Compose3 [] I  I  Char" $ roundTripEq (undefined :: LogScaled (Compose3 [] I  I  Char))
      , testProperty "Compose3 [] [] I  Char" $ roundTripEq (undefined :: LogScaled (Compose3 [] [] I  Char))
      , testProperty "Compose3 [] I  [] Char" $ roundTripEq (undefined :: LogScaled (Compose3 [] I  [] Char))
      , testProperty "Compose3 [] [] [] Char" $ roundTripEq (undefined :: LogScaled (Compose3 [] [] [] Char))

      , testProperty "Compose3' I  I  I  Char" $ roundTripEq (undefined :: LogScaled (Compose3' I  I  I  Char))
      , testProperty "Compose3' I  [] I  Char" $ roundTripEq (undefined :: LogScaled (Compose3' I  [] I  Char))
      , testProperty "Compose3' I  I  [] Char" $ roundTripEq (undefined :: LogScaled (Compose3' I  I  [] Char))
      , testProperty "Compose3' I  [] [] Char" $ roundTripEq (undefined :: LogScaled (Compose3' I  [] [] Char))
      , testProperty "Compose3' [] I  I  Char" $ roundTripEq (undefined :: LogScaled (Compose3' [] I  I  Char))
      , testProperty "Compose3' [] [] I  Char" $ roundTripEq (undefined :: LogScaled (Compose3' [] [] I  Char))
      , testProperty "Compose3' [] I  [] Char" $ roundTripEq (undefined :: LogScaled (Compose3' [] I  [] Char))
      , testProperty "Compose3' [] [] [] Char" $ roundTripEq (undefined :: LogScaled (Compose3' [] [] [] Char))
      ]
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
    , testProperty "[Text]" $ roundTripKey (undefined :: LogScaled [T.Text])
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
    , testProperty "Version" $ roundTripKey (undefined :: Version)
    , testProperty "Lazy Text" $ roundTripKey (undefined :: LT.Text)
    , testProperty "UUID" $ roundTripKey UUID.nil
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
    , testProperty "parserThrowError" parserThrowErrorProp
    , testProperty "parserCatchError" parserCatchErrorProp
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
      , testGroup "EitherTextInt" [
          testProperty "UntaggedValue" (isUntaggedValueETI . gEitherTextIntToJSONUntaggedValue)
        , testProperty "roundtrip" (toParseJSON gEitherTextIntParseJSONUntaggedValue gEitherTextIntToJSONUntaggedValue)
        ]
      , testGroup "SomeType" [
          testProperty "2ElemArray" (is2ElemArray . gSomeTypeToJSON2ElemArray)
        , testProperty "TaggedObject" (isTaggedObject . gSomeTypeToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . gSomeTypeToJSONObjectWithSingleField)
        , testGroup "roundTrip" [
            testProperty "2ElemArray" (toParseJSON gSomeTypeParseJSON2ElemArray gSomeTypeToJSON2ElemArray)
          , testProperty "TaggedObject" (toParseJSON gSomeTypeParseJSONTaggedObject gSomeTypeToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (toParseJSON gSomeTypeParseJSONObjectWithSingleField gSomeTypeToJSONObjectWithSingleField)

#if __GLASGOW_HASKELL__ >= 706
          , testProperty "2ElemArray unary" (toParseJSON1 gSomeTypeLiftParseJSON2ElemArray gSomeTypeLiftToJSON2ElemArray)
          , testProperty "TaggedObject unary" (toParseJSON1 gSomeTypeLiftParseJSONTaggedObject gSomeTypeLiftToJSONTaggedObject)
          , testProperty "ObjectWithSingleField unary" (toParseJSON1 gSomeTypeLiftParseJSONObjectWithSingleField gSomeTypeLiftToJSONObjectWithSingleField)
#endif
          ]
        ]
      , testGroup "OneConstructor" [
          testProperty "default" (isEmptyArray . gOneConstructorToJSONDefault)
        , testProperty "Tagged"  (isTaggedObject . gOneConstructorToJSONTagged)
        , testGroup "roundTrip" [
            testProperty "default" (toParseJSON gOneConstructorParseJSONDefault gOneConstructorToJSONDefault)
          , testProperty "Tagged"  (toParseJSON gOneConstructorParseJSONTagged  gOneConstructorToJSONTagged)
          ]
        ]
      , testGroup "OptionField" [
          testProperty "like Maybe" $
          \x -> gOptionFieldToJSON (OptionField (Option x)) === thMaybeFieldToJSON (MaybeField x)
        , testProperty "roundTrip" (toParseJSON gOptionFieldParseJSON gOptionFieldToJSON)
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

      , testProperty "EitherTextInt UntaggedValue" $
        gEitherTextIntToJSONUntaggedValue `sameAs` gEitherTextIntToEncodingUntaggedValue

      , testProperty "SomeType2ElemArray" $
        gSomeTypeToJSON2ElemArray `sameAs` gSomeTypeToEncoding2ElemArray
#if __GLASGOW_HASKELL__ >= 706
      , testProperty "SomeType2ElemArray unary" $
        gSomeTypeLiftToJSON2ElemArray `sameAs1` gSomeTypeLiftToEncoding2ElemArray
      , testProperty "SomeType2ElemArray unary agree" $
        gSomeTypeToEncoding2ElemArray `sameAs1Agree` gSomeTypeLiftToEncoding2ElemArray
#endif

      , testProperty "SomeTypeTaggedObject" $
        gSomeTypeToJSONTaggedObject `sameAs` gSomeTypeToEncodingTaggedObject
#if __GLASGOW_HASKELL__ >= 706
      , testProperty "SomeTypeTaggedObject unary" $
        gSomeTypeLiftToJSONTaggedObject `sameAs1` gSomeTypeLiftToEncodingTaggedObject
      , testProperty "SomeTypeTaggedObject unary agree" $
        gSomeTypeToEncodingTaggedObject `sameAs1Agree` gSomeTypeLiftToEncodingTaggedObject
#endif

      , testProperty "SomeTypeObjectWithSingleField" $
        gSomeTypeToJSONObjectWithSingleField `sameAs` gSomeTypeToEncodingObjectWithSingleField
#if __GLASGOW_HASKELL__ >= 706
      , testProperty "SomeTypeObjectWithSingleField unary" $
        gSomeTypeLiftToJSONObjectWithSingleField `sameAs1` gSomeTypeLiftToEncodingObjectWithSingleField
      , testProperty "SomeTypeObjectWithSingleField unary agree" $
        gSomeTypeToEncodingObjectWithSingleField `sameAs1Agree` gSomeTypeLiftToEncodingObjectWithSingleField
#endif

      , testProperty "SomeTypeOmitNothingFields" $
        gSomeTypeToJSONOmitNothingFields `sameAs` gSomeTypeToEncodingOmitNothingFields

      , testProperty "OneConstructorDefault" $
        gOneConstructorToJSONDefault `sameAs` gOneConstructorToEncodingDefault
      , testProperty "OneConstructorTagged" $
        gOneConstructorToJSONTagged `sameAs` gOneConstructorToEncodingTagged

      , testProperty "OptionField" $
        gOptionFieldToJSON `sameAs` gOptionFieldToEncoding
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
      , testGroup "EitherTextInt" [
          testProperty "UntaggedValue" (isUntaggedValueETI . thEitherTextIntToJSONUntaggedValue)
        , testProperty "roundtrip" (toParseJSON thEitherTextIntParseJSONUntaggedValue thEitherTextIntToJSONUntaggedValue)
        ]
      , testGroup "SomeType" [
          testProperty "2ElemArray" (is2ElemArray . thSomeTypeToJSON2ElemArray)
        , testProperty "TaggedObject" (isTaggedObject . thSomeTypeToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thSomeTypeToJSONObjectWithSingleField)
        , testGroup "roundTrip" [
            testProperty "2ElemArray" (toParseJSON thSomeTypeParseJSON2ElemArray thSomeTypeToJSON2ElemArray)
          , testProperty "TaggedObject" (toParseJSON thSomeTypeParseJSONTaggedObject thSomeTypeToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (toParseJSON thSomeTypeParseJSONObjectWithSingleField thSomeTypeToJSONObjectWithSingleField)

          , testProperty "2ElemArray unary" (toParseJSON1 thSomeTypeLiftParseJSON2ElemArray thSomeTypeLiftToJSON2ElemArray)
          , testProperty "TaggedObject unary" (toParseJSON1 thSomeTypeLiftParseJSONTaggedObject thSomeTypeLiftToJSONTaggedObject)
          , testProperty "ObjectWithSingleField unary" (toParseJSON1 thSomeTypeLiftParseJSONObjectWithSingleField thSomeTypeLiftToJSONObjectWithSingleField)

          ]
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
      , testGroup "OneConstructor" [
          testProperty "default" (isEmptyArray . thOneConstructorToJSONDefault)
        , testProperty "Tagged"  (isTaggedObject . thOneConstructorToJSONTagged)
        , testGroup "roundTrip" [
            testProperty "default" (toParseJSON thOneConstructorParseJSONDefault thOneConstructorToJSONDefault)
          , testProperty "Tagged"  (toParseJSON thOneConstructorParseJSONTagged  thOneConstructorToJSONTagged)
          ]
        ]
      , testGroup "OptionField" [
          testProperty "like Maybe" $
          \x -> thOptionFieldToJSON (OptionField (Option x)) === thMaybeFieldToJSON (MaybeField x)
        , testProperty "roundTrip" (toParseJSON thOptionFieldParseJSON thOptionFieldToJSON)
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

      , testProperty "EitherTextInt UntaggedValue" $
        thEitherTextIntToJSONUntaggedValue `sameAs` thEitherTextIntToEncodingUntaggedValue

      , testProperty "SomeType2ElemArray" $
        thSomeTypeToJSON2ElemArray `sameAs` thSomeTypeToEncoding2ElemArray
      , testProperty "SomeType2ElemArray unary" $
        thSomeTypeLiftToJSON2ElemArray `sameAs1` thSomeTypeLiftToEncoding2ElemArray
      , testProperty "SomeType2ElemArray unary agree" $
        thSomeTypeToEncoding2ElemArray `sameAs1Agree` thSomeTypeLiftToEncoding2ElemArray

      , testProperty "SomeTypeTaggedObject" $
        thSomeTypeToJSONTaggedObject `sameAs` thSomeTypeToEncodingTaggedObject
      , testProperty "SomeTypeTaggedObject unary" $
        thSomeTypeLiftToJSONTaggedObject `sameAs1` thSomeTypeLiftToEncodingTaggedObject
      , testProperty "SomeTypeTaggedObject unary agree" $
        thSomeTypeToEncodingTaggedObject `sameAs1Agree` thSomeTypeLiftToEncodingTaggedObject

      , testProperty "SomeTypeObjectWithSingleField" $
        thSomeTypeToJSONObjectWithSingleField `sameAs` thSomeTypeToEncodingObjectWithSingleField
      , testProperty "SomeTypeObjectWithSingleField unary" $
        thSomeTypeLiftToJSONObjectWithSingleField `sameAs1` thSomeTypeLiftToEncodingObjectWithSingleField
      , testProperty "SomeTypeObjectWithSingleField unary agree" $
        thSomeTypeToEncodingObjectWithSingleField `sameAs1Agree` thSomeTypeLiftToEncodingObjectWithSingleField

      , testProperty "OneConstructorDefault" $
        thOneConstructorToJSONDefault `sameAs` thOneConstructorToEncodingDefault
      , testProperty "OneConstructorTagged" $
        thOneConstructorToJSONTagged `sameAs` thOneConstructorToEncodingTagged

      , testProperty "OptionField" $
        thOptionFieldToJSON `sameAs` thOptionFieldToEncoding
      ]
    ]
  ]
