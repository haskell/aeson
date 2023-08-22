{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module PropUtils (
    encodeInteger,
    encodeDouble,
    toParseJSON,
    toParseJSON1,
    roundTripEq,
    roundTripKey,
    roundtripReadShow,
    toFromJSON,
    sameAs,
    sameAs1,
    sameAs1Agree,
    modifyFailureProp,
    parserThrowErrorProp,
    parserCatchErrorProp,
    -- * Predicates
    isEmptyArray,
    isTaggedObject,
    isString,
    isObjectWithSingleField,
    is2ElemArray,
    isNullaryTaggedObject,
    isUntaggedValueETI,
) where

import Prelude.Compat

import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Types
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Encoders
import Instances ()
import Test.QuickCheck (Arbitrary(..), Property, Testable, (===), (.&&.), counterexample, property)
import Types
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector as V
import qualified Data.Aeson.Decoding as Dec


encodeDouble :: Double -> Double -> Property
encodeDouble num denom
    | isNaN d      = encode d === "null"
    | isInfinite d = if d > 0 then encode d === "\"+inf\"" else encode d === "\"-inf\""
    | otherwise    = (read . L.unpack . encode) d === d
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
    parsejson = parsejson1 omittedField parseJSON (listParser parseJSON)
    tojson    = tojson1 omitField toJSON (listValue toJSON)

roundTripEnc :: (FromJSON a, ToJSON a, Show a) =>
             (a -> a -> Property) -> a -> Property
roundTripEnc eq i =
    case eitherDecode . encode $ i of
      Right v  -> v `eq` i
      Left err -> failure "parsing" err i

roundTripDecEnc :: (FromJSON a, ToJSON a, Show a) =>
             (a -> a -> Property) -> a -> Property
roundTripDecEnc eq i =
    case Dec.eitherDecodeStrict . L.toStrict . encode $ i of
      Right v      -> v `eq` i
      Left err     -> failure "parse" err i

roundTripNoEnc :: (FromJSON a, ToJSON a, Show a) =>
             (a -> a -> Property) -> a -> Property
roundTripNoEnc eq i =
    case ifromJSON . toJSON $ i of
      (ISuccess v)      -> v `eq` i
      (IError path err) -> failure "fromJSON" (formatError path err) i

roundTripOmit :: (FromJSON a, ToJSON a, Show a) =>
             (Maybe a -> Maybe a -> Property) -> a -> Property
roundTripOmit eq i
    | omitField i = omf `eq` Just i
    | otherwise   = case fmap omitField omf of
        Nothing -> property True
        Just True -> property True
        Just False -> counterexample (show omf) False
  where
    omf = omittedField

roundTripEq :: (Eq a, FromJSON a, ToJSON a, Show a) => a -> Property
roundTripEq y =
  roundTripEnc (===) y .&&.
  roundTripNoEnc (===) y .&&.
  roundTripDecEnc (===) y .&&.
  roundTripOmit (===) y

roundtripReadShow :: Value -> Property
roundtripReadShow v = readMaybe (show v) === Just v

-- We test keys by encoding HashMap and Map with it
roundTripKey
    :: (Ord a, Hashable a, FromJSONKey a, ToJSONKey a, Show a)
    => HashMap a Int -> Map a Int -> Property
roundTripKey h m = roundTripEq h .&&. roundTripEq m

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
    result === Success ([Key "outer", Key "inner"] ++ jsonPath, msg)
  where
    parser = parserCatchError outer (curry pure)

    outer = inner <?> Key "outer"
    inner = parserThrowError jsonPath msg <?> Key "inner"

    result :: Result (JSONPath, String)
    result = parse (const parser) ()

    jsonPath = map (Key . Key.fromString) path

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
    rhs = Right $ toVal1 omitField toJSON (listValue toJSON) v
    lhs = eitherDecode . encodingToLazyByteString $
        toEnc1 omitField toEncoding (listEncoding toEncoding) v

sameAs1Agree
    :: ToJSON a
    => (f a -> Encoding)
    -> (forall b. LiftToEncoding f b)
    -> f a
    -> Property
sameAs1Agree toEnc toEnc1 v = rhs === lhs
  where
    rhs = encodingToLazyByteString $ toEnc v
    lhs = encodingToLazyByteString $ toEnc1 omitField toEncoding (listEncoding toEncoding) v

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

{-
isTaggedObjectValue :: Value -> Bool
isTaggedObjectValue (Object obj) = "tag"      `KM.member` obj &&
                                   "contents" `KM.member` obj
isTaggedObjectValue _            = False
-}

isNullaryTaggedObject :: Value -> Bool
isNullaryTaggedObject obj = isTaggedObject' obj && isObjectWithSingleField obj

isTaggedObject :: Value -> Property
isTaggedObject = checkValue isTaggedObject'

isTaggedObject' :: Value -> Bool
isTaggedObject' (Object obj) = "tag" `KM.member` obj
isTaggedObject' _            = False

isObjectWithSingleField :: Value -> Bool
isObjectWithSingleField (Object obj) = KM.size obj == 1
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
