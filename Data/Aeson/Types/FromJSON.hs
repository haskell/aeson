{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

#include "incoherent-compat.h"
#include "overlapping-compat.h"

-- TODO: Drop this when we remove support for Data.Attoparsec.Number
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Data.Aeson.Types.FromJSON
    (
    -- * Core JSON classes
      FromJSON(..)
    -- * Liftings to unary and binary type constructors
    , FromJSON1(..)
    , parseJSON1
    , FromJSON2(..)
    , parseJSON2
    -- * Generic JSON classes
    , GFromJSON(..)
    , FromArgs(..)
    , genericParseJSON
    , genericLiftParseJSON
    -- * Classes and types for map keys
    , FromJSONKey(..)
    , FromJSONKeyFunction(..)
    , fromJSONKeyCoerce
    , coerceFromJSONKeyFunction
    , mapFromJSONKeyFunction

    -- * List functions
    , listParser

    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withNumber
    , withScientific
    , withBool
    , withEmbeddedJSON

    -- * Functions
    , fromJSON
    , ifromJSON
    , typeMismatch
    , parseField
    , parseFieldMaybe
    , parseFieldMaybe'
    , explicitParseField
    , explicitParseFieldMaybe
    , explicitParseFieldMaybe'
    -- ** Operators
    , (.:)
    , (.:?)
    , (.:!)
    , (.!=)

    -- * Internal
    , parseOptionalFieldWith
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative ((<|>), Const(..))
import Control.Monad ((<=<), zipWithM)
import Data.Aeson.Internal.Functions (mapKey)
import Data.Aeson.Parser.Internal (eitherDecodeWith, jsonEOF)
import Data.Aeson.Types.Generic
import Data.Aeson.Types.Internal
import Data.Attoparsec.Number (Number(..))
import Data.Bits (unsafeShiftR)
import Data.Fixed (Fixed, HasResolution)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.Hashable (Hashable(..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Ratio ((%), Ratio)
import Data.Scientific (Scientific)
import Data.Tagged (Tagged(..))
import Data.Text (Text, pack, unpack)
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Time.Format (parseTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Traversable as Tr (sequence)
import Data.Vector (Vector)
import Data.Version (Version, parseVersion)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable)
import Foreign.C.Types (CTime (..))
import GHC.Generics
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP (readP_to_S)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Aeson.Compat as Compat
import qualified Data.Aeson.Parser.Time as Time
import qualified Data.Attoparsec.ByteString.Char8 as A (endOfInput, parseOnly, scientific)
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Monoid as Monoid
import qualified Data.Scientific as Scientific
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Tree as Tree
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

#ifndef HAS_COERCIBLE
#define HAS_COERCIBLE (__GLASGOW_HASKELL__ >= 707)
#endif

#if HAS_COERCIBLE
import Data.Coerce (Coercible, coerce)
coerce' :: Coercible a b => a -> b
coerce' = coerce
#else
coerce' :: a -> b
coerce' = unsafeCoerce
#endif

parseIndexedJSON :: (Value -> Parser a) -> Int -> Value -> Parser a
parseIndexedJSON p idx value = p value <?> Index idx
{-# INLINE parseIndexedJSON #-}

parseIndexedJSONPair :: (Value -> Parser a) -> (Value -> Parser b) -> Int -> Value -> Parser (a, b)
parseIndexedJSONPair keyParser valParser idx value = p value <?> Index idx
  where
    p = withArray "(k,v)" $ \ab ->
        let n = V.length ab
        in if n == 2
             then (,) <$> parseJSONElemAtIndex keyParser 0 ab
                      <*> parseJSONElemAtIndex valParser 1 ab
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a pair"
{-# INLINE parseIndexedJSONPair #-}

parseJSONElemAtIndex :: (Value -> Parser a) -> Int -> V.Vector Value -> Parser a
parseJSONElemAtIndex p idx ary = p (V.unsafeIndex ary idx) <?> Index idx

scientificToNumber :: Scientific -> Number
scientificToNumber s
    | e < 0     = D $ Scientific.toRealFloat s
    | otherwise = I $ c * 10 ^ e
  where
    e = Scientific.base10Exponent s
    c = Scientific.coefficient s
{-# INLINE scientificToNumber #-}

parseRealFloat :: RealFloat a => String -> Value -> Parser a
parseRealFloat _        (Number s) = pure $ Scientific.toRealFloat s
parseRealFloat _        Null       = pure (0/0)
parseRealFloat expected v          = typeMismatch expected v
{-# INLINE parseRealFloat #-}

parseIntegralFromScientific :: forall a. Integral a => String -> Scientific -> Parser a
parseIntegralFromScientific expected s =
    case Scientific.floatingOrInteger s :: Either Double a of
        Right x -> pure x
        Left _  -> fail $ "expected " ++ expected ++ ", encountered floating number " ++ show s
{-# INLINE parseIntegralFromScientific #-}

parseIntegral :: Integral a => String -> Value -> Parser a
parseIntegral expected =
    withScientific expected $ parseIntegralFromScientific expected
{-# INLINE parseIntegral #-}

parseBoundedIntegralFromScientific :: (Bounded a, Integral a) => String -> Scientific -> Parser a
parseBoundedIntegralFromScientific expected s = maybe
    (fail $ expected ++ " is either floating or will cause over or underflow: " ++ show s)
    pure
    (Scientific.toBoundedInteger s)
{-# INLINE parseBoundedIntegralFromScientific #-}

parseBoundedIntegral :: (Bounded a, Integral a) => String -> Value -> Parser a
parseBoundedIntegral expected =
    withScientific expected $ parseBoundedIntegralFromScientific expected
{-# INLINE parseBoundedIntegral #-}

parseScientificText :: Text -> Parser Scientific
parseScientificText
    = either fail pure
    . A.parseOnly (A.scientific <* A.endOfInput)
    . T.encodeUtf8

parseIntegralText :: Integral a => String -> Text -> Parser a
parseIntegralText expected t =
    parseScientificText t >>= parseIntegralFromScientific expected
{-# INLINE parseIntegralText #-}

parseBoundedIntegralText :: (Bounded a, Integral a) => String -> Text -> Parser a
parseBoundedIntegralText expected t =
    parseScientificText t >>= parseBoundedIntegralFromScientific expected

parseOptionalFieldWith :: (Value -> Parser (Maybe a))
                       -> Object -> Text -> Parser (Maybe a)
parseOptionalFieldWith pj obj key =
    case H.lookup key obj of
     Nothing -> pure Nothing
     Just v  -> pj v <?> Key key
{-# INLINE parseOptionalFieldWith #-}

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Class of generic representation types that can be converted from JSON.
class GFromJSON arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'parseJSON' (if the @arity@ is 'Zero')
    -- or 'liftParseJSON' (if the @arity@ is 'One').
    gParseJSON :: Options -> FromArgs arity a -> Value -> Parser (f a)

-- | A 'FromArgs' value either stores nothing (for 'FromJSON') or it stores the
-- two function arguments that decode occurrences of the type parameter (for
-- 'FromJSON1').
data FromArgs arity a where
    NoFromArgs :: FromArgs Zero a
    From1Args  :: (Value -> Parser a) -> (Value -> Parser [a]) -> FromArgs One a

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'parseJSON' when the
-- type is an instance of 'Generic'.
genericParseJSON :: (Generic a, GFromJSON Zero (Rep a))
                 => Options -> Value -> Parser a
genericParseJSON opts = fmap to . gParseJSON opts NoFromArgs

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftParseJSON' when the
-- type is an instance of 'Generic1'.
genericLiftParseJSON :: (Generic1 f, GFromJSON One (Rep1 f))
                     => Options -> (Value -> Parser a) -> (Value -> Parser [a])
                     -> Value -> Parser (f a)
genericLiftParseJSON opts pj pjl = fmap to1 . gParseJSON opts (From1Args pj pjl)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | A type that can be converted from JSON, with the possibility of
-- failure.
--
-- In many cases, you can get the compiler to generate parsing code
-- for you (see below).  To begin, let's cover writing an instance by
-- hand.
--
-- There are various reasons a conversion could fail.  For example, an
-- 'Object' could be missing a required key, an 'Array' could be of
-- the wrong size, or a value could be of an incompatible type.
--
-- The basic ways to signal a failed conversion are as follows:
--
-- * 'empty' and 'mzero' work, but are terse and uninformative;
--
-- * 'fail' yields a custom error message;
--
-- * 'typeMismatch' produces an informative message for cases when the
-- value encountered is not of the expected type.
--
-- An example type and instance using 'typeMismatch':
--
-- @
-- \-- Allow ourselves to write 'Text' literals.
-- {-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance 'FromJSON' Coord where
--     'parseJSON' ('Object' v) = Coord
--         '<$>' v '.:' \"x\"
--         '<*>' v '.:' \"y\"
--
--     \-- We do not expect a non-'Object' value here.
--     \-- We could use 'mzero' to fail, but 'typeMismatch'
--     \-- gives a much more informative error message.
--     'parseJSON' invalid    = 'typeMismatch' \"Coord\" invalid
-- @
--
-- For this common case of only being concerned with a single
-- type of JSON value, the functions 'withObject', 'withNumber', etc.
-- are provided. Their use is to be preferred when possible, since
-- they are more terse. Using 'withObject', we can rewrite the above instance
-- (assuming the same language extension and data type) as:
--
-- @
-- instance 'FromJSON' Coord where
--     'parseJSON' = 'withObject' \"Coord\" $ \\v -> Coord
--         '<$>' v '.:' \"x\"
--         '<*>' v '.:' \"y\"
-- @
--
-- Instead of manually writing your 'FromJSON' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so it will probably be more efficient than the following option.
--
-- * The compiler can provide a default generic implementation for
-- 'parseJSON'.
--
-- To use the second, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a 'FromJSON' instance for your datatype without giving
-- a definition for 'parseJSON'.
--
-- For example, the previous example can be simplified to just:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Coord = Coord { x :: Double, y :: Double } deriving 'Generic'
--
-- instance 'FromJSON' Coord
-- @
--
-- The default implementation will be equivalent to
-- @parseJSON = 'genericParseJSON' 'defaultOptions'@; If you need different
-- options, you can customize the generic decoding by defining:
--
-- @
-- customOptions = 'defaultOptions'
--                 { 'fieldLabelModifier' = 'map' 'Data.Char.toUpper'
--                 }
--
-- instance 'FromJSON' Coord where
--     'parseJSON' = 'genericParseJSON' customOptions
-- @
class FromJSON a where
    parseJSON :: Value -> Parser a

    default parseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
    parseJSON = genericParseJSON defaultOptions

    parseJSONList :: Value -> Parser [a]
    parseJSONList (Array a)
        = zipWithM (parseIndexedJSON parseJSON) [0..]
        . V.toList
        $ a

    parseJSONList v = typeMismatch "[a]" v

-------------------------------------------------------------------------------
--  Classes and types for map keys
-------------------------------------------------------------------------------

-- | Read the docs for 'ToJSONKey' first. This class is a conversion
--   in the opposite direction. If you have a newtype wrapper around 'Text',
--   the recommended way to define instances is with generalized newtype deriving:
--
--   > newtype SomeId = SomeId { getSomeId :: Text }
--   >   deriving (Eq,Ord,Hashable,FromJSONKey)
--
class FromJSONKey a where
    -- | Strategy for parsing the key of a map-like container.
    fromJSONKey :: FromJSONKeyFunction a
    default fromJSONKey :: FromJSON a => FromJSONKeyFunction a
    fromJSONKey = FromJSONKeyValue parseJSON

    -- | This is similar in spirit to the 'readList' method of 'Read'.
    --   It makes it possible to give 'String' keys special treatment
    --   without using @OverlappingInstances@. End users should always
    --   be able to use the default implementation of this method.
    fromJSONKeyList :: FromJSONKeyFunction [a]
    default fromJSONKeyList :: FromJSON a => FromJSONKeyFunction [a]
    fromJSONKeyList = FromJSONKeyValue parseJSON

-- | With GHC 7.8+ we carry around @'Coercible' 'Text' a@ dictionary,
-- to give us an assurance that the program will not segfault.
-- Unfortunately we cannot enforce that the 'Eq' instances or the
-- 'Hashable' instances for 'Text' and @a@ agree.
--
-- At the moment this type is intentionally not exported. 'FromJSONKeyFunction'
-- can be inspected, but cannot be constructed.
data CoerceText a where
#if HAS_COERCIBLE
    CoerceText :: Coercible Text a => CoerceText a
#else
    CoerceText :: CoerceText a
#endif

-- | This type is related to 'ToJSONKeyFunction'. If 'FromJSONKeyValue' is used in the
--   'FromJSONKey' instance, then 'ToJSONKeyValue' should be used in the 'ToJSONKey'
--   instance. The other three data constructors for this type all correspond to
--   'ToJSONKeyText'. Strictly speaking, 'FromJSONKeyTextParser' is more powerful than
--   'FromJSONKeyText', which is in turn more powerful than 'FromJSONKeyCoerce'.
--   For performance reasons, these exist as three options instead of one.
data FromJSONKeyFunction a
    = FromJSONKeyCoerce !(CoerceText a)
      -- ^ uses 'coerce' ('unsafeCoerce' in older GHCs)
    | FromJSONKeyText !(Text -> a)
      -- ^ conversion from 'Text' that always succeeds
    | FromJSONKeyTextParser !(Text -> Parser a)
      -- ^ conversion from 'Text' that may fail
    | FromJSONKeyValue !(Value -> Parser a)
      -- ^ conversion for non-textual keys

-- | Only law abiding up to interpretation
instance Functor FromJSONKeyFunction where
    fmap h (FromJSONKeyCoerce CoerceText) = FromJSONKeyText (h . coerce')
    fmap h (FromJSONKeyText f)            = FromJSONKeyText (h . f)
    fmap h (FromJSONKeyTextParser f)      = FromJSONKeyTextParser (fmap h . f)
    fmap h (FromJSONKeyValue f)           = FromJSONKeyValue (fmap h . f)

-- | Construct 'FromJSONKeyFunction' for types coercible from 'Text'. This
-- conversion is still unsafe, as 'Hashable' and 'Eq' instances of @a@ should be
-- compatible with 'Text' i.e. hash values should be equal for wrapped values as well.
-- This property will always be maintained if the 'Hashable' and 'Eq' instances
-- are derived with generalized newtype deriving.
-- compatible with 'Text' i.e. hash values be equal for wrapped values as well.
--
-- On pre GHC 7.8 this is unconstrainted function.
fromJSONKeyCoerce ::
#if HAS_COERCIBLE
    Coercible Text a =>
#endif
    FromJSONKeyFunction a
fromJSONKeyCoerce = FromJSONKeyCoerce CoerceText

-- | Semantically the same as @coerceFromJSONKeyFunction = fmap coerce = coerce@.
--
-- See note on 'fromJSONKeyCoerce'.
coerceFromJSONKeyFunction ::
#if HAS_COERCIBLE
    Coercible a b =>
#endif
    FromJSONKeyFunction a -> FromJSONKeyFunction b
#if HAS_COERCIBLE
coerceFromJSONKeyFunction = coerce
#else
coerceFromJSONKeyFunction (FromJSONKeyCoerce CoerceText) = FromJSONKeyCoerce CoerceText
coerceFromJSONKeyFunction (FromJSONKeyText f)            = FromJSONKeyText (coerce' . f)
coerceFromJSONKeyFunction (FromJSONKeyTextParser f)      = FromJSONKeyTextParser (fmap coerce' . f)
coerceFromJSONKeyFunction (FromJSONKeyValue f)           = FromJSONKeyValue (fmap coerce' . f)
#endif

{-# RULES
  "FromJSONKeyCoerce: fmap id"     forall (x :: FromJSONKeyFunction a).
                                   fmap id x = x
  #-}
#if HAS_COERCIBLE
{-# RULES
  "FromJSONKeyCoerce: fmap coerce" forall x .
                                   fmap coerce x = coerceFromJSONKeyFunction x
  #-}
#endif

-- | Same as 'fmap'. Provided for the consistency with 'ToJSONKeyFunction'.
mapFromJSONKeyFunction :: (a -> b) -> FromJSONKeyFunction a -> FromJSONKeyFunction b
mapFromJSONKeyFunction = fmap

-------------------------------------------------------------------------------
-- Functions needed for documentation
-------------------------------------------------------------------------------

-- | Fail parsing due to a type mismatch, with a descriptive message.
--
-- Example usage:
--
-- @
-- instance FromJSON Coord where
--   parseJSON ('Object' v) = {- type matches, life is good -}
--   parseJSON wat        = 'typeMismatch' \"Coord\" wat
-- @
typeMismatch :: String -- ^ The name of the type you are trying to parse.
             -> Value  -- ^ The actual value encountered.
             -> Parser a
typeMismatch expected actual =
    fail $ "expected " ++ expected ++ ", encountered " ++ name
  where
    name = case actual of
             Object _ -> "Object"
             Array _  -> "Array"
             String _ -> "String"
             Number _ -> "Number"
             Bool _   -> "Boolean"
             Null     -> "Null"

-------------------------------------------------------------------------------
-- Lifings of FromJSON and ToJSON to unary and binary type constructors
-------------------------------------------------------------------------------

-- | Lifting of the 'FromJSON' class to unary type constructors.
--
-- Instead of manually writing your 'FromJSON1' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so it will probably be more efficient than the following option.
--
-- * The compiler can provide a default generic implementation for
-- 'liftParseJSON'.
--
-- To use the second, simply add a @deriving 'Generic1'@ clause to your
-- datatype and declare a 'FromJSON1' instance for your datatype without giving
-- a definition for 'liftParseJSON'.
--
-- For example:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Pair a b = Pair { pairFst :: a, pairSnd :: b } deriving 'Generic1'
--
-- instance 'FromJSON' a => 'FromJSON1' (Pair a)
-- @
--
-- If the default implementation doesn't give exactly the results you want,
-- you can customize the generic decoding with only a tiny amount of
-- effort, using 'genericLiftParseJSON' with your preferred 'Options':
--
-- @
-- customOptions = 'defaultOptions'
--                 { 'fieldLabelModifier' = 'map' 'Data.Char.toUpper'
--                 }
--
-- instance 'FromJSON' a => 'FromJSON1' (Pair a) where
--     'liftParseJSON' = 'genericLiftParseJSON' customOptions
-- @
class FromJSON1 f where
    liftParseJSON :: (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser (f a)

    default liftParseJSON :: (Generic1 f, GFromJSON One (Rep1 f))
                          => (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser (f a)
    liftParseJSON = genericLiftParseJSON defaultOptions

    liftParseJSONList :: (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser [f a]
    liftParseJSONList f g v = listParser (liftParseJSON f g) v

-- | Lift the standard 'parseJSON' function through the type constructor.
parseJSON1 :: (FromJSON1 f, FromJSON a) => Value -> Parser (f a)
parseJSON1 = liftParseJSON parseJSON parseJSONList
{-# INLINE parseJSON1 #-}

-- | Lifting of the 'FromJSON' class to binary type constructors.
--
-- Instead of manually writing your 'FromJSON2' instance, "Data.Aeson.TH"
-- provides Template Haskell functions which will derive an instance at compile time.

-- The compiler cannot provide a default generic implementation for 'liftParseJSON2',
-- unlike 'parseJSON' and 'liftParseJSON'.
class FromJSON2 f where
    liftParseJSON2
        :: (Value -> Parser a)
        -> (Value -> Parser [a])
        -> (Value -> Parser b)
        -> (Value -> Parser [b])
        -> Value -> Parser (f a b)
    liftParseJSONList2
        :: (Value -> Parser a)
        -> (Value -> Parser [a])
        -> (Value -> Parser b)
        -> (Value -> Parser [b])
        -> Value -> Parser [f a b]
    liftParseJSONList2 fa ga fb gb v = case v of
        Array vals -> fmap V.toList (V.mapM (liftParseJSON2 fa ga fb gb) vals)
        _ -> typeMismatch "[a]" v

-- | Lift the standard 'parseJSON' function through the type constructor.
parseJSON2 :: (FromJSON2 f, FromJSON a, FromJSON b) => Value -> Parser (f a b)
parseJSON2 = liftParseJSON2 parseJSON parseJSONList parseJSON parseJSONList
{-# INLINE parseJSON2 #-}

-------------------------------------------------------------------------------
-- List functions
-------------------------------------------------------------------------------

-- | Helper function to use with 'liftParseJSON'. See 'Data.Aeson.ToJSON.listEncoding'.
listParser :: (Value -> Parser a) -> Value -> Parser [a]
listParser f (Array xs) = fmap V.toList (V.mapM f xs)
listParser _ v          = typeMismatch "[a]" v
{-# INLINE listParser #-}

-------------------------------------------------------------------------------
-- [] instances
-------------------------------------------------------------------------------

instance FromJSON1 [] where
    liftParseJSON _ p' = p'
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON [a] where
    parseJSON = parseJSON1

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | @'withObject' expected f value@ applies @f@ to the 'Object' when @value@
--   is an 'Object' and fails using @'typeMismatch' expected@ otherwise.
withObject :: String -> (Object -> Parser a) -> Value -> Parser a
withObject _        f (Object obj) = f obj
withObject expected _ v            = typeMismatch expected v
{-# INLINE withObject #-}

-- | @'withText' expected f value@ applies @f@ to the 'Text' when @value@ is a
--   'String' and fails using @'typeMismatch' expected@ otherwise.
withText :: String -> (Text -> Parser a) -> Value -> Parser a
withText _        f (String txt) = f txt
withText expected _ v            = typeMismatch expected v
{-# INLINE withText #-}

-- | @'withArray' expected f value@ applies @f@ to the 'Array' when @value@ is
-- an 'Array' and fails using @'typeMismatch' expected@ otherwise.
withArray :: String -> (Array -> Parser a) -> Value -> Parser a
withArray _        f (Array arr) = f arr
withArray expected _ v           = typeMismatch expected v
{-# INLINE withArray #-}

-- | @'withNumber' expected f value@ applies @f@ to the 'Number' when @value@
-- is a 'Number' and fails using @'typeMismatch' expected@ otherwise.
withNumber :: String -> (Number -> Parser a) -> Value -> Parser a
withNumber expected f = withScientific expected (f . scientificToNumber)
{-# INLINE withNumber #-}
{-# DEPRECATED withNumber "Use withScientific instead" #-}

-- | @'withScientific' expected f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Number' and fails using @'typeMismatch' expected@
-- otherwise.
withScientific :: String -> (Scientific -> Parser a) -> Value -> Parser a
withScientific _        f (Number scientific) = f scientific
withScientific expected _ v                   = typeMismatch expected v
{-# INLINE withScientific #-}

-- | @'withBool' expected f value@ applies @f@ to the 'Bool' when @value@ is a
-- 'Bool' and fails using @'typeMismatch' expected@ otherwise.
withBool :: String -> (Bool -> Parser a) -> Value -> Parser a
withBool _        f (Bool arr) = f arr
withBool expected _ v          = typeMismatch expected v
{-# INLINE withBool #-}

-- | Decode a nested JSON-encoded string.
withEmbeddedJSON :: (FromJSON a) => String -> (Value -> Parser a) -> Value -> Parser a
withEmbeddedJSON _ innerParser (String txt) =
    either fail innerParser $ eitherDecode (Compat.fromStrict $ T.encodeUtf8 txt)
    where
        eitherDecode = eitherFormatError . eitherDecodeWith jsonEOF ifromJSON
        eitherFormatError = either (Left . uncurry formatError) Right
withEmbeddedJSON name _ v = typeMismatch name v
{-# INLINE withEmbeddedJSON #-}

-- | Convert a value from JSON, failing if the types do not match.
fromJSON :: (FromJSON a) => Value -> Result a
fromJSON = parse parseJSON
{-# INLINE fromJSON #-}

-- | Convert a value from JSON, failing if the types do not match.
ifromJSON :: (FromJSON a) => Value -> IResult a
ifromJSON = iparse parseJSON
{-# INLINE ifromJSON #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
(.:) :: (FromJSON a) => Object -> Text -> Parser a
(.:) = explicitParseField parseJSON
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is 'Null',
-- or 'empty' if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
(.:?) = explicitParseFieldMaybe parseJSON
{-# INLINE (.:?) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present or 'empty' if the
-- value cannot be converted to the desired type.
--
-- This differs from '.:?' by attempting to parse 'Null' the same as any
-- other JSON value, instead of interpreting it as 'Nothing'.
(.:!) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
(.:!) = explicitParseFieldMaybe' parseJSON
{-# INLINE (.:!) #-}

-- | Function variant of '.:'.
parseField :: (FromJSON a) => Object -> Text -> Parser a
parseField = (.:)
{-# INLINE parseField #-}

-- | Function variant of '.:?'.
parseFieldMaybe :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
parseFieldMaybe = (.:?)
{-# INLINE parseFieldMaybe #-}

-- | Function variant of '.:!'.
parseFieldMaybe' :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
parseFieldMaybe' = (.:!)
{-# INLINE parseFieldMaybe' #-}

-- | Variant of '.:' with explicit parser function.
--
-- E.g. @'explicitParseField' 'parseJSON1' :: ('FromJSON1' f, 'FromJSON' a) -> 'Object' -> 'Text' -> 'Parser' (f a)@
explicitParseField :: (Value -> Parser a) -> Object -> Text -> Parser a
explicitParseField p obj key = case H.lookup key obj of
    Nothing -> fail $ "key " ++ show key ++ " not present"
    Just v  -> p v <?> Key key
{-# INLINE explicitParseField #-}

-- | Variant of '.:?' with explicit parser function.
explicitParseFieldMaybe :: (Value -> Parser a) -> Object -> Text -> Parser (Maybe a)
explicitParseFieldMaybe p obj key = case H.lookup key obj of
    Nothing -> pure Nothing
    Just v  -> liftParseJSON p (listParser p) v <?> Key key -- listParser isn't used by maybe instance.
{-# INLINE explicitParseFieldMaybe #-}

-- | Variant of '.:!' with explicit parser function.
explicitParseFieldMaybe' :: (Value -> Parser a) -> Object -> Text -> Parser (Maybe a)
explicitParseFieldMaybe' p obj key = case H.lookup key obj of
    Nothing -> pure Nothing
    Just v  -> Just <$> p v <?> Key key
{-# INLINE explicitParseFieldMaybe' #-}

-- | Helper for use in combination with '.:?' to provide default
-- values for optional JSON object fields.
--
-- This combinator is most useful if the key and value can be absent
-- from an object without affecting its validity and we know a default
-- value to assign in that case.  If the key and value are mandatory,
-- use '.:' instead.
--
-- Example usage:
--
-- @ v1 <- o '.:?' \"opt_field_with_dfl\" .!= \"default_val\"
-- v2 <- o '.:'  \"mandatory_field\"
-- v3 <- o '.:?' \"opt_field2\"
-- @
(.!=) :: Parser (Maybe a) -> a -> Parser a
pmval .!= val = fromMaybe val <$> pmval
{-# INLINE (.!=) #-}

--------------------------------------------------------------------------------
-- Generic parseJSON
-------------------------------------------------------------------------------

instance OVERLAPPABLE_ (GFromJSON arity a) => GFromJSON arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is just added to the
    -- parsed value:
    gParseJSON opts fargs = fmap M1 . gParseJSON opts fargs

instance (FromJSON a) => GFromJSON arity (K1 i a) where
    -- Constant values are decoded using their FromJSON instance:
    gParseJSON _opts _ = fmap K1 . parseJSON

instance GFromJSON One Par1 where
    -- Direct occurrences of the last type parameter are decoded with the
    -- function passed in as an argument:
    gParseJSON _opts (From1Args pj _) = fmap Par1 . pj

instance (FromJSON1 f) => GFromJSON One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are decoded using their
    -- FromJSON1 instance:
    gParseJSON _opts (From1Args pj pjl) = fmap Rec1 . liftParseJSON pj pjl

instance GFromJSON arity U1 where
    -- Empty constructors are expected to be encoded as an empty array:
    gParseJSON _opts _ v
        | isEmptyArray v = pure U1
        | otherwise      = typeMismatch "unit constructor (U1)" v

instance ( ConsFromJSON arity a
         , AllNullary         (C1 c a) allNullary
         , ParseSum     arity (C1 c a) allNullary
         ) => GFromJSON arity (D1 d (C1 c a)) where
    -- The option 'tagSingleConstructors' determines whether to wrap
    -- a single-constructor type.
    gParseJSON opts fargs
        | tagSingleConstructors opts
            = fmap M1
            . (unTagged :: Tagged allNullary (Parser (C1 c a p)) -> Parser (C1 c a p))
            . parseSum opts fargs
        | otherwise = fmap M1 . fmap M1 . consParseJSON opts fargs

instance (ConsFromJSON arity a) => GFromJSON arity (C1 c a) where
    -- Constructors need to be decoded differently depending on whether they're
    -- a record or not. This distinction is made by consParseJSON:
    gParseJSON opts fargs = fmap M1 . consParseJSON opts fargs

instance ( FromProduct arity a, FromProduct arity b
         , ProductSize       a, ProductSize       b
         ) => GFromJSON arity (a :*: b) where
    -- Products are expected to be encoded to an array. Here we check whether we
    -- got an array of the same size as the product, then parse each of the
    -- product's elements using parseProduct:
    gParseJSON opts fargs = withArray "product (:*:)" $ \arr ->
      let lenArray = V.length arr
          lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
                       productSize in
      if lenArray == lenProduct
      then parseProduct opts fargs arr 0 lenProduct
      else fail $ "When expecting a product of " ++ show lenProduct ++
                  " values, encountered an Array of " ++ show lenArray ++
                  " elements instead"

instance ( AllNullary         (a :+: b) allNullary
         , ParseSum     arity (a :+: b) allNullary
         ) => GFromJSON arity (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are expected to be
    -- encoded as strings.  This distinction is made by 'parseSum':
    gParseJSON opts fargs =
      (unTagged :: Tagged allNullary (Parser ((a :+: b) d)) ->
                                     Parser ((a :+: b) d))
                 . parseSum opts fargs

instance (FromJSON1 f, GFromJSON One g) => GFromJSON One (f :.: g) where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is decoded by using the outermost type's FromJSON1
    -- instance to generically decode the innermost type:
    gParseJSON opts fargs =
      let gpj = gParseJSON opts fargs in
      fmap Comp1 . liftParseJSON gpj (listParser gpj)

--------------------------------------------------------------------------------

class ParseSum arity f allNullary where
    parseSum :: Options -> FromArgs arity a
             -> Value -> Tagged allNullary (Parser (f a))

instance ( SumFromString           f
         , FromPair          arity f
         , FromTaggedObject  arity f
         , FromUntaggedValue arity f
         ) => ParseSum       arity f True where
    parseSum opts fargs
        | allNullaryToStringTag opts = Tagged . parseAllNullarySum    opts
        | otherwise                  = Tagged . parseNonAllNullarySum opts fargs

instance ( FromPair          arity f
         , FromTaggedObject  arity f
         , FromUntaggedValue arity f
         ) => ParseSum       arity f False where
    parseSum opts fargs = Tagged . parseNonAllNullarySum opts fargs

--------------------------------------------------------------------------------

parseAllNullarySum :: SumFromString f => Options -> Value -> Parser (f a)
parseAllNullarySum opts = withText "Text" $ \key ->
                            maybe (notFound key) return $
                              parseSumFromString opts key

class SumFromString f where
    parseSumFromString :: Options -> Text -> Maybe (f a)

instance (SumFromString a, SumFromString b) => SumFromString (a :+: b) where
    parseSumFromString opts key = (L1 <$> parseSumFromString opts key) <|>
                                  (R1 <$> parseSumFromString opts key)

instance (Constructor c) => SumFromString (C1 c U1) where
    parseSumFromString opts key | key == name = Just $ M1 U1
                                | otherwise   = Nothing
        where
          name = pack $ constructorTagModifier opts $
                          conName (undefined :: t c U1 p)

--------------------------------------------------------------------------------

parseNonAllNullarySum :: ( FromPair          arity f
                         , FromTaggedObject  arity f
                         , FromUntaggedValue arity f
                         ) => Options -> FromArgs arity c
                           -> Value -> Parser (f c)
parseNonAllNullarySum opts fargs =
    case sumEncoding opts of
      TaggedObject{..} ->
          withObject "Object" $ \obj -> do
            tag <- obj .: pack tagFieldName
            fromMaybe (notFound tag) $
              parseFromTaggedObject opts fargs contentsFieldName obj tag

      ObjectWithSingleField ->
          withObject "Object" $ \obj ->
            case H.toList obj of
              [pair@(tag, _)] -> fromMaybe (notFound tag) $
                                   parsePair opts fargs pair
              _ -> fail "Object doesn't have a single field"

      TwoElemArray ->
          withArray "Array" $ \arr ->
            if V.length arr == 2
            then case V.unsafeIndex arr 0 of
                   String tag -> fromMaybe (notFound tag) $
                                   parsePair opts fargs (tag, V.unsafeIndex arr 1)
                   _ -> fail "First element is not a String"
            else fail "Array doesn't have 2 elements"

      UntaggedValue -> parseUntaggedValue opts fargs

--------------------------------------------------------------------------------

class FromTaggedObject arity f where
    parseFromTaggedObject :: Options -> FromArgs arity a
                          -> String -> Object
                          -> Text -> Maybe (Parser (f a))

instance ( FromTaggedObject arity a, FromTaggedObject arity b) =>
    FromTaggedObject arity (a :+: b) where
        parseFromTaggedObject opts fargs contentsFieldName obj tag =
            (fmap L1 <$> parseFromTaggedObject opts fargs contentsFieldName obj tag) <|>
            (fmap R1 <$> parseFromTaggedObject opts fargs contentsFieldName obj tag)

instance ( FromTaggedObject' arity f
         , Constructor c
         ) => FromTaggedObject arity (C1 c f) where
    parseFromTaggedObject opts fargs contentsFieldName obj tag
        | tag == name = Just $ M1 <$> parseFromTaggedObject'
                                        opts fargs contentsFieldName obj
        | otherwise = Nothing
        where
          name = pack $ constructorTagModifier opts $
                          conName (undefined :: t c f p)

--------------------------------------------------------------------------------

class FromTaggedObject' arity f where
    parseFromTaggedObject' :: Options -> FromArgs arity a -> String
                           -> Object -> Parser (f a)

class FromTaggedObject'' arity f isRecord where
    parseFromTaggedObject'' :: Options -> FromArgs arity a -> String
                            -> Object -> Tagged isRecord (Parser (f a))

instance ( IsRecord                   f isRecord
         , FromTaggedObject''   arity f isRecord
         ) => FromTaggedObject' arity f where
    parseFromTaggedObject' opts fargs contentsFieldName =
        (unTagged :: Tagged isRecord (Parser (f a)) -> Parser (f a)) .
        parseFromTaggedObject'' opts fargs contentsFieldName

instance (FromRecord arity f) => FromTaggedObject'' arity f True where
    parseFromTaggedObject'' opts fargs _ =
      Tagged . parseRecord opts fargs Nothing

instance (GFromJSON arity f) => FromTaggedObject'' arity f False where
    parseFromTaggedObject'' opts fargs contentsFieldName = Tagged .
      (gParseJSON opts fargs <=< (.: pack contentsFieldName))

instance OVERLAPPING_ FromTaggedObject'' arity U1 False where
    parseFromTaggedObject'' _ _ _ _ = Tagged (pure U1)

--------------------------------------------------------------------------------

class ConsFromJSON arity f where
    consParseJSON  :: Options -> FromArgs arity a
                   -> Value -> Parser (f a)

class ConsFromJSON' arity f isRecord where
    consParseJSON' :: Options -> FromArgs arity a
                   -> Maybe Text -- ^ A dummy label
                                 --   (Nothing to use proper label)
                   -> Value -> Tagged isRecord (Parser (f a))

instance ( IsRecord            f isRecord
         , ConsFromJSON' arity f isRecord
         ) => ConsFromJSON arity f where
    consParseJSON opts fargs v = let
      (v2,lab) = case (unwrapUnaryRecords opts,isUnary (undefined :: f a)) of
                       -- use a dummy object with a dummy label
        (True,True) -> (object [(pack "dummy",v)], Just $ pack "dummy")
        _ ->(v,Nothing)
      in (unTagged :: Tagged isRecord (Parser (f a)) -> Parser (f a))
                       $ consParseJSON' opts fargs lab v2


instance (FromRecord arity f) => ConsFromJSON' arity f True where
    consParseJSON' opts fargs mlab = Tagged . withObject "record (:*:)"
                                        (parseRecord opts fargs mlab)

instance (GFromJSON arity f) => ConsFromJSON' arity f False where
    consParseJSON' opts fargs _ = Tagged . gParseJSON opts fargs

--------------------------------------------------------------------------------

class FromRecord arity f where
    parseRecord :: Options -> FromArgs arity a
                -> Maybe Text -- ^ A dummy label
                              --   (Nothing to use proper label)
                -> Object -> Parser (f a)

instance ( FromRecord arity a
         , FromRecord arity b
         ) => FromRecord arity (a :*: b) where
    parseRecord opts fargs _ obj =
      (:*:) <$> parseRecord opts fargs Nothing obj
            <*> parseRecord opts fargs Nothing obj

instance OVERLAPPABLE_ (Selector s, GFromJSON arity a) =>
  FromRecord arity (S1 s a) where
    parseRecord opts fargs lab =
      (<?> Key label) . gParseJSON opts fargs <=< (.: label)
        where
          label = fromMaybe defLabel lab
          defLabel = pack . fieldLabelModifier opts $
                       selName (undefined :: t s a p)

instance INCOHERENT_ (Selector s, FromJSON a) =>
  FromRecord arity (S1 s (K1 i (Maybe a))) where
    parseRecord _ _ (Just lab) obj = (M1 . K1) <$> obj .:? lab
    parseRecord opts _ Nothing obj = (M1 . K1) <$> obj .:? pack label
        where
          label = fieldLabelModifier opts $
                    selName (undefined :: t s (K1 i (Maybe a)) p)

-- Parse an Option like a Maybe.
instance INCOHERENT_ (Selector s, FromJSON a) =>
  FromRecord arity (S1 s (K1 i (Semigroup.Option a))) where
    parseRecord opts fargs lab obj = wrap <$> parseRecord opts fargs lab obj
      where
        wrap :: S1 s (K1 i (Maybe a)) p -> S1 s (K1 i (Semigroup.Option a)) p
        wrap (M1 (K1 a)) = M1 (K1 (Semigroup.Option a))

--------------------------------------------------------------------------------

class FromProduct arity f where
    parseProduct :: Options -> FromArgs arity a
                 -> Array -> Int -> Int
                 -> Parser (f a)

instance ( FromProduct    arity a
         , FromProduct    arity b
         ) => FromProduct arity (a :*: b) where
    parseProduct opts fargs arr ix len =
        (:*:) <$> parseProduct opts fargs arr ix  lenL
              <*> parseProduct opts fargs arr ixR lenR
        where
          lenL = len `unsafeShiftR` 1
          ixR  = ix + lenL
          lenR = len - lenL

instance (GFromJSON arity a) => FromProduct arity (S1 s a) where
    parseProduct opts fargs arr ix _ =
      gParseJSON opts fargs $ V.unsafeIndex arr ix

--------------------------------------------------------------------------------

class FromPair arity f where
    parsePair :: Options -> FromArgs arity a
              -> Pair -> Maybe (Parser (f a))

instance ( FromPair arity a
         , FromPair arity b
         ) => FromPair arity (a :+: b) where
    parsePair opts fargs pair = (fmap L1 <$> parsePair opts fargs pair) <|>
                                (fmap R1 <$> parsePair opts fargs pair)

instance ( Constructor c
         , GFromJSON    arity a
         , ConsFromJSON arity a
         ) => FromPair arity (C1 c a) where
    parsePair opts fargs (tag, value)
        | tag == tag' = Just $ gParseJSON opts fargs value
        | otherwise   = Nothing
        where
          tag' = pack $ constructorTagModifier opts $
                          conName (undefined :: t c a p)

--------------------------------------------------------------------------------

class FromUntaggedValue arity f where
    parseUntaggedValue :: Options -> FromArgs arity a
                       -> Value -> Parser (f a)

instance
    ( FromUntaggedValue    arity a
    , FromUntaggedValue    arity b
    ) => FromUntaggedValue arity (a :+: b)
  where
    parseUntaggedValue opts fargs value =
        L1 <$> parseUntaggedValue opts fargs value <|>
        R1 <$> parseUntaggedValue opts fargs value

instance OVERLAPPABLE_
    ( GFromJSON            arity a
    , ConsFromJSON         arity a
    ) => FromUntaggedValue arity (C1 c a)
  where
    parseUntaggedValue = gParseJSON

instance OVERLAPPING_
    ( Constructor c )
    => FromUntaggedValue arity (C1 c U1)
  where
    parseUntaggedValue opts _ (String s)
        | s == pack (constructorTagModifier opts (conName (undefined :: t c U1 p))) =
            pure $ M1 U1
        | otherwise =
            fail $ "Invalid tag: " ++ unpack s
    parseUntaggedValue _ _ v = typeMismatch (conName (undefined :: t c U1 p)) v


--------------------------------------------------------------------------------

notFound :: Text -> Parser a
notFound key = fail $ "The key \"" ++ unpack key ++ "\" was not found"
{-# INLINE notFound #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------


instance FromJSON2 Const where
    liftParseJSON2 p _ _ _ = fmap Const . p
    {-# INLINE liftParseJSON2 #-}

instance FromJSON a => FromJSON1 (Const a) where
    liftParseJSON _ _ = fmap Const . parseJSON
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Const a b) where
    {-# INLINE parseJSON #-}
    parseJSON = fmap Const . parseJSON


instance FromJSON1 Maybe where
    liftParseJSON _ _ Null = pure Nothing
    liftParseJSON p _ a    = Just <$> p a
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Maybe a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}



instance FromJSON2 Either where
    liftParseJSON2 pA _ pB _ (Object (H.toList -> [(key, value)]))
        | key == left  = Left  <$> pA value <?> Key left
        | key == right = Right <$> pB value <?> Key right
      where
        left, right :: Text
        left  = "Left"
        right = "Right"

    liftParseJSON2 _ _ _ _ _ = fail $
        "expected an object with a single property " ++
        "where the property key should be either " ++
        "\"Left\" or \"Right\""
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a) => FromJSON1 (Either a) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}



instance FromJSON Bool where
    parseJSON = withBool "Bool" pure
    {-# INLINE parseJSON #-}

instance FromJSONKey Bool where
    fromJSONKey = FromJSONKeyTextParser $ \t -> case t of
        "true"  -> pure True
        "false" -> pure False
        _       -> fail $ "Cannot parse key into Bool: " ++ T.unpack t

instance FromJSON Ordering where
  parseJSON = withText "Ordering" $ \s ->
    case s of
      "LT" -> return LT
      "EQ" -> return EQ
      "GT" -> return GT
      _ -> fail "Parsing Ordering value failed: expected \"LT\", \"EQ\", or \"GT\""

instance FromJSON () where
    parseJSON = withArray "()" $ \v ->
                  if V.null v
                    then pure ()
                    else fail "Expected an empty array"
    {-# INLINE parseJSON #-}

instance FromJSON Char where
    parseJSON = withText "Char" $ \t ->
                  if T.compareLength t 1 == EQ
                    then pure $ T.head t
                    else fail "Expected a string of length 1"
    {-# INLINE parseJSON #-}

    parseJSONList = withText "String" $ pure . T.unpack
    {-# INLINE parseJSONList #-}

instance FromJSON Double where
    parseJSON = parseRealFloat "Double"
    {-# INLINE parseJSON #-}

instance FromJSONKey Double where
    fromJSONKey = FromJSONKeyTextParser $ \t -> case t of
        "NaN"       -> pure (0/0)
        "Infinity"  -> pure (1/0)
        "-Infinity" -> pure (negate 1/0)
        _           -> Scientific.toRealFloat <$> parseScientificText t

instance FromJSON Number where
    parseJSON (Number s) = pure $ scientificToNumber s
    parseJSON Null       = pure (D (0/0))
    parseJSON v          = typeMismatch "Number" v
    {-# INLINE parseJSON #-}

instance FromJSON Float where
    parseJSON = parseRealFloat "Float"
    {-# INLINE parseJSON #-}

instance FromJSONKey Float where
    fromJSONKey = FromJSONKeyTextParser $ \t -> case t of
        "NaN"       -> pure (0/0)
        "Infinity"  -> pure (1/0)
        "-Infinity" -> pure (negate 1/0)
        _           -> Scientific.toRealFloat <$> parseScientificText t

instance (FromJSON a, Integral a) => FromJSON (Ratio a) where
    parseJSON = withObject "Rational" $ \obj ->
                  (%) <$> obj .: "numerator"
                      <*> obj .: "denominator"
    {-# INLINE parseJSON #-}

-- | /WARNING:/ Only parse fixed-precision numbers from trusted input
-- since an attacker could easily fill up the memory of the target
-- system by specifying a scientific number with a big exponent like
-- @1e1000000000@.
instance HasResolution a => FromJSON (Fixed a) where
    parseJSON = withScientific "Fixed" $ pure . realToFrac
    {-# INLINE parseJSON #-}

instance FromJSON Int where
    parseJSON = parseBoundedIntegral "Int"
    {-# INLINE parseJSON #-}

instance FromJSONKey Int where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int"

-- | /WARNING:/ Only parse Integers from trusted input since an
-- attacker could easily fill up the memory of the target system by
-- specifying a scientific number with a big exponent like
-- @1e1000000000@.
instance FromJSON Integer where
    parseJSON = parseIntegral "Integer"
    {-# INLINE parseJSON #-}

instance FromJSONKey Integer where
    fromJSONKey = FromJSONKeyTextParser $ parseIntegralText "Integer"

instance FromJSON Natural where
    parseJSON value = do
        integer :: Integer <- parseIntegral "Natural" value
        if integer < 0 then
            fail $ "expected Natural, encountered negative number " <> show integer
        else
            pure $ fromIntegral integer

instance FromJSONKey Natural where
    fromJSONKey = FromJSONKeyTextParser $ \text -> do
        integer :: Integer <- parseIntegralText "Natural" text
        if integer < 0 then
            fail $ "expected Natural, encountered negative number " <> show integer
        else
            pure $ fromIntegral integer

instance FromJSON Int8 where
    parseJSON = parseBoundedIntegral "Int8"
    {-# INLINE parseJSON #-}

instance FromJSONKey Int8 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int8"

instance FromJSON Int16 where
    parseJSON = parseBoundedIntegral "Int16"
    {-# INLINE parseJSON #-}

instance FromJSONKey Int16 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int16"

instance FromJSON Int32 where
    parseJSON = parseBoundedIntegral "Int32"
    {-# INLINE parseJSON #-}

instance FromJSONKey Int32 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int32"

instance FromJSON Int64 where
    parseJSON = parseBoundedIntegral "Int64"
    {-# INLINE parseJSON #-}

instance FromJSONKey Int64 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int64"

instance FromJSON Word where
    parseJSON = parseBoundedIntegral "Word"
    {-# INLINE parseJSON #-}

instance FromJSONKey Word where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word"

instance FromJSON Word8 where
    parseJSON = parseBoundedIntegral "Word8"
    {-# INLINE parseJSON #-}

instance FromJSONKey Word8 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word8"

instance FromJSON Word16 where
    parseJSON = parseBoundedIntegral "Word16"
    {-# INLINE parseJSON #-}

instance FromJSONKey Word16 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word16"

instance FromJSON Word32 where
    parseJSON = parseBoundedIntegral "Word32"
    {-# INLINE parseJSON #-}

instance FromJSONKey Word32 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word32"

instance FromJSON Word64 where
    parseJSON = parseBoundedIntegral "Word64"
    {-# INLINE parseJSON #-}

instance FromJSONKey Word64 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word64"

instance FromJSON CTime where
    parseJSON = fmap CTime . parseJSON
    {-# INLINE parseJSON #-}

instance FromJSON Text where
    parseJSON = withText "Text" pure
    {-# INLINE parseJSON #-}

instance FromJSONKey Text where
    fromJSONKey = fromJSONKeyCoerce


instance FromJSON LT.Text where
    parseJSON = withText "Lazy Text" $ pure . LT.fromStrict
    {-# INLINE parseJSON #-}

instance FromJSONKey LT.Text where
    fromJSONKey = FromJSONKeyText LT.fromStrict


instance FromJSON Version where
    parseJSON = withText "Version" parseVersionText
    {-# INLINE parseJSON #-}

instance FromJSONKey Version where
    fromJSONKey = FromJSONKeyTextParser parseVersionText

parseVersionText :: Text -> Parser Version
parseVersionText = go . readP_to_S parseVersion . unpack
  where
    go [(v,[])] = return v
    go (_ : xs) = go xs
    go _        = fail "could not parse Version"

-------------------------------------------------------------------------------
-- semigroups NonEmpty
-------------------------------------------------------------------------------

instance FromJSON1 NonEmpty where
    liftParseJSON p _ = withArray "NonEmpty a" $
        (>>= ne) . Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
      where
        ne []     = fail "Expected a NonEmpty but got an empty list"
        ne (x:xs) = pure (x :| xs)
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (NonEmpty a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance FromJSON Scientific where
    parseJSON = withScientific "Scientific" pure
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- DList
-------------------------------------------------------------------------------

instance FromJSON1 DList.DList where
    liftParseJSON p _ = withArray "DList a" $
      fmap DList.fromList .
      Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (DList.DList a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- tranformers - Functors
-------------------------------------------------------------------------------

instance FromJSON1 Identity where
    liftParseJSON p _ a = Identity <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Identity <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Identity a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}

instance (FromJSONKey a) => FromJSONKey (Identity a) where
    fromJSONKey = coerceFromJSONKeyFunction (fromJSONKey :: FromJSONKeyFunction a)
    fromJSONKeyList = coerceFromJSONKeyFunction (fromJSONKeyList :: FromJSONKeyFunction [a])


instance (FromJSON1 f, FromJSON1 g) => FromJSON1 (Compose f g) where
    liftParseJSON p pl a = Compose <$> liftParseJSON g gl a
      where
        g  = liftParseJSON p pl
        gl = liftParseJSONList p pl
    {-# INLINE liftParseJSON #-}

    liftParseJSONList p pl a = map Compose <$> liftParseJSONList g gl a
      where
        g  = liftParseJSON p pl
        gl = liftParseJSONList p pl
    {-# INLINE liftParseJSONList #-}

instance (FromJSON1 f, FromJSON1 g, FromJSON a) => FromJSON (Compose f g a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance (FromJSON1 f, FromJSON1 g) => FromJSON1 (Product f g) where
    liftParseJSON p pl a = uncurry Pair <$> liftParseJSON2 px pxl py pyl a
      where
        px  = liftParseJSON p pl
        pxl = liftParseJSONList p pl
        py  = liftParseJSON p pl
        pyl = liftParseJSONList p pl
    {-# INLINE liftParseJSON #-}

instance (FromJSON1 f, FromJSON1 g, FromJSON a) => FromJSON (Product f g a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance (FromJSON1 f, FromJSON1 g) => FromJSON1 (Sum f g) where
    liftParseJSON p pl (Object (H.toList -> [(key, value)]))
        | key == inl = InL <$> liftParseJSON p pl value <?> Key inl
        | key == inr = InR <$> liftParseJSON p pl value <?> Key inl
      where
        inl, inr :: Text
        inl = "InL"
        inr = "InR"

    liftParseJSON _ _ _ = fail $
        "expected an object with a single property " ++
        "where the property key should be either " ++
        "\"InL\" or \"InR\""
    {-# INLINE liftParseJSON #-}

instance (FromJSON1 f, FromJSON1 g, FromJSON a) => FromJSON (Sum f g a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance FromJSON1 Seq.Seq where
    liftParseJSON p _ = withArray "Seq a" $
      fmap Seq.fromList .
      Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Seq.Seq a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance (Ord a, FromJSON a) => FromJSON (Set.Set a) where
    parseJSON = fmap Set.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance FromJSON IntSet.IntSet where
    parseJSON = fmap IntSet.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance FromJSON1 IntMap.IntMap where
    liftParseJSON p pl = fmap IntMap.fromList . liftParseJSON p' pl'
      where
        p'  = liftParseJSON2     parseJSON parseJSONList p pl
        pl' = liftParseJSONList2 parseJSON parseJSONList p pl
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (IntMap.IntMap a) where
    parseJSON = fmap IntMap.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance (FromJSONKey k, Ord k) => FromJSON1 (M.Map k) where
    liftParseJSON p _ = case fromJSONKey of
        FromJSONKeyCoerce _-> withObject "Map k v" $
            fmap (H.foldrWithKey (M.insert . unsafeCoerce) M.empty) . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyText f -> withObject "Map k v" $
            fmap (H.foldrWithKey (M.insert . f) M.empty) . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyTextParser f -> withObject "Map k v" $
            H.foldrWithKey (\k v m -> M.insert <$> f k <?> Key k <*> p v <?> Key k <*> m) (pure M.empty)
        FromJSONKeyValue f -> withArray "Map k v" $ \arr ->
            fmap M.fromList . Tr.sequence .
                zipWith (parseIndexedJSONPair f p) [0..] . V.toList $ arr
    {-# INLINE liftParseJSON #-}

instance (FromJSONKey k, Ord k, FromJSON v) => FromJSON (M.Map k v) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance FromJSON1 Tree.Tree where
    liftParseJSON p pl = go
      where
        go v = uncurry Tree.Node <$> liftParseJSON2 p pl p' pl' v

        p' = liftParseJSON go (listParser go)
        pl'= liftParseJSONList go (listParser go)

instance (FromJSON v) => FromJSON (Tree.Tree v) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- uuid
-------------------------------------------------------------------------------

instance FromJSON UUID.UUID where
    parseJSON = withText "UUID" $
        maybe (fail "Invalid UUID") pure . UUID.fromText

instance FromJSONKey UUID.UUID where
    fromJSONKey = FromJSONKeyTextParser $
        maybe (fail "Invalid UUID") pure . UUID.fromText

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance FromJSON1 Vector where
    liftParseJSON p _ = withArray "Vector a" $
        V.mapM (uncurry $ parseIndexedJSON p) . V.indexed
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Vector a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


vectorParseJSON :: (FromJSON a, VG.Vector w a) => String -> Value -> Parser (w a)
vectorParseJSON s = withArray s $ fmap V.convert . V.mapM (uncurry $ parseIndexedJSON parseJSON) . V.indexed
{-# INLINE vectorParseJSON #-}

instance (Storable a, FromJSON a) => FromJSON (VS.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Storable.Vector a"

instance (VP.Prim a, FromJSON a) => FromJSON (VP.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Primitive.Vector a"
    {-# INLINE parseJSON #-}

instance (VG.Vector VU.Vector a, FromJSON a) => FromJSON (VU.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Unboxed.Vector a"
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance (Eq a, Hashable a, FromJSON a) => FromJSON (HashSet.HashSet a) where
    parseJSON = fmap HashSet.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance (FromJSONKey k, Eq k, Hashable k) => FromJSON1 (H.HashMap k) where
    liftParseJSON p _ = case fromJSONKey of
        FromJSONKeyCoerce _ -> withObject "HashMap ~Text v" $
            uc . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyText f -> withObject "HashMap k v" $
            fmap (mapKey f) . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyTextParser f -> withObject "HashMap k v" $
            H.foldrWithKey (\k v m -> H.insert <$> f k <?> Key k <*> p v <?> Key k <*> m) (pure H.empty)
        FromJSONKeyValue f -> withArray "Map k v" $ \arr ->
            fmap H.fromList . Tr.sequence .
                zipWith (parseIndexedJSONPair f p) [0..] . V.toList $ arr
      where
        uc :: Parser (H.HashMap Text v) -> Parser (H.HashMap k v)
        uc = unsafeCoerce

instance (FromJSON v, FromJSONKey k, Eq k, Hashable k) => FromJSON (H.HashMap k v) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance FromJSON Value where
    parseJSON = pure
    {-# INLINE parseJSON #-}

instance FromJSON DotNetTime where
    parseJSON = withText "DotNetTime" $ \t ->
        let (s,m) = T.splitAt (T.length t - 5) t
            t'    = T.concat [s,".",m]
        in case parseTime defaultTimeLocale "/Date(%s%Q)/" (unpack t') of
             Just d -> pure (DotNetTime d)
             _      -> fail "could not parse .NET time"
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance FromJSON Day where
    parseJSON = withText "Day" (Time.run Time.day)

instance FromJSONKey Day where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.day)


instance FromJSON TimeOfDay where
    parseJSON = withText "TimeOfDay" (Time.run Time.timeOfDay)

instance FromJSONKey TimeOfDay where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.timeOfDay)


instance FromJSON LocalTime where
    parseJSON = withText "LocalTime" (Time.run Time.localTime)

instance FromJSONKey LocalTime where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.localTime)


-- | Supported string formats:
--
-- @YYYY-MM-DD HH:MM Z@
-- @YYYY-MM-DD HH:MM:SS Z@
-- @YYYY-MM-DD HH:MM:SS.SSS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
instance FromJSON ZonedTime where
    parseJSON = withText "ZonedTime" (Time.run Time.zonedTime)

instance FromJSONKey ZonedTime where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.zonedTime)


instance FromJSON UTCTime where
    parseJSON = withText "UTCTime" (Time.run Time.utcTime)

instance FromJSONKey UTCTime where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.utcTime)


-- | /WARNING:/ Only parse lengths of time from trusted input
-- since an attacker could easily fill up the memory of the target
-- system by specifying a scientific number with a big exponent like
-- @1e1000000000@.
instance FromJSON NominalDiffTime where
    parseJSON = withScientific "NominalDiffTime" $ pure . realToFrac
    {-# INLINE parseJSON #-}


-- | /WARNING:/ Only parse lengths of time from trusted input
-- since an attacker could easily fill up the memory of the target
-- system by specifying a scientific number with a big exponent like
-- @1e1000000000@.
instance FromJSON DiffTime where
    parseJSON = withScientific "DiffTime" $ pure . realToFrac
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- base Monoid/Semigroup
-------------------------------------------------------------------------------

instance FromJSON1 Monoid.Dual where
    liftParseJSON p _ = fmap Monoid.Dual . p
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Monoid.Dual a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance FromJSON1 Monoid.First where
    liftParseJSON p p' = fmap Monoid.First . liftParseJSON p p'
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Monoid.First a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance FromJSON1 Monoid.Last where
    liftParseJSON p p' = fmap Monoid.Last . liftParseJSON p p'
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Monoid.Last a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance FromJSON1 Semigroup.Min where
    liftParseJSON p _ a = Semigroup.Min <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Semigroup.Min <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Semigroup.Min a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance FromJSON1 Semigroup.Max where
    liftParseJSON p _ a = Semigroup.Max <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Semigroup.Max <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Semigroup.Max a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance FromJSON1 Semigroup.First where
    liftParseJSON p _ a = Semigroup.First <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Semigroup.First <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Semigroup.First a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance FromJSON1 Semigroup.Last where
    liftParseJSON p _ a = Semigroup.Last <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Semigroup.Last <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Semigroup.Last a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance FromJSON1 Semigroup.WrappedMonoid where
    liftParseJSON p _ a = Semigroup.WrapMonoid <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Semigroup.WrapMonoid <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Semigroup.WrappedMonoid a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance FromJSON1 Semigroup.Option where
    liftParseJSON p p' = fmap Semigroup.Option . liftParseJSON p p'
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Semigroup.Option a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance FromJSON1 Proxy where
    {-# INLINE liftParseJSON #-}
    liftParseJSON _ _ Null = pure Proxy
    liftParseJSON _ _ v    = typeMismatch "Proxy" v

instance FromJSON (Proxy a) where
    {-# INLINE parseJSON #-}
    parseJSON Null = pure Proxy
    parseJSON v    = typeMismatch "Proxy" v


instance FromJSON2 Tagged where
    liftParseJSON2 _ _ p _ = fmap Tagged . p
    {-# INLINE liftParseJSON2 #-}

instance FromJSON1 (Tagged a) where
    liftParseJSON p _ = fmap Tagged . p
    {-# INLINE liftParseJSON #-}

instance FromJSON b => FromJSON (Tagged a b) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

instance FromJSONKey b => FromJSONKey (Tagged a b) where
    fromJSONKey = coerceFromJSONKeyFunction (fromJSONKey :: FromJSONKeyFunction b)
    fromJSONKeyList = (fmap . fmap) Tagged fromJSONKeyList

-------------------------------------------------------------------------------
-- Instances for converting from map keys
-------------------------------------------------------------------------------

instance (FromJSON a, FromJSON b) => FromJSONKey (a,b)
instance (FromJSON a, FromJSON b, FromJSON c) => FromJSONKey (a,b,c)
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSONKey (a,b,c,d)

instance FromJSONKey Char where
    fromJSONKey = FromJSONKeyTextParser $ \t ->
        if T.length t == 1
            then return (T.index t 0)
            else typeMismatch "Expected Char but String didn't contain exactly one character" (String t)
    fromJSONKeyList = FromJSONKeyText T.unpack

instance (FromJSONKey a, FromJSON a) => FromJSONKey [a] where
    fromJSONKey = fromJSONKeyList

-------------------------------------------------------------------------------
-- Tuple instances, see tuple-instances-from.hs
-------------------------------------------------------------------------------

instance FromJSON2 (,) where
    liftParseJSON2 pA _ pB _ = withArray "(a, b)" $ \t ->
        let n = V.length t
        in if n == 2
            then (,)
                <$> parseJSONElemAtIndex pA 0 t
                <*> parseJSONElemAtIndex pB 1 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 2"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a) => FromJSON1 ((,) a) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (a, b) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a) => FromJSON2 ((,,) a) where
    liftParseJSON2 pB _ pC _ = withArray "(a, b, c)" $ \t ->
        let n = V.length t
        in if n == 3
            then (,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex pB 1 t
                <*> parseJSONElemAtIndex pC 2 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 3"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b) => FromJSON1 ((,,) a b) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a, b, c) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b) => FromJSON2 ((,,,) a b) where
    liftParseJSON2 pC _ pD _ = withArray "(a, b, c, d)" $ \t ->
        let n = V.length t
        in if n == 4
            then (,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex pC 2 t
                <*> parseJSONElemAtIndex pD 3 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 4"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON1 ((,,,) a b c) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (a, b, c, d) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON2 ((,,,,) a b c) where
    liftParseJSON2 pD _ pE _ = withArray "(a, b, c, d, e)" $ \t ->
        let n = V.length t
        in if n == 5
            then (,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex pD 3 t
                <*> parseJSONElemAtIndex pE 4 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 5"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON1 ((,,,,) a b c d) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) => FromJSON (a, b, c, d, e) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON2 ((,,,,,) a b c d) where
    liftParseJSON2 pE _ pF _ = withArray "(a, b, c, d, e, f)" $ \t ->
        let n = V.length t
        in if n == 6
            then (,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex pE 4 t
                <*> parseJSONElemAtIndex pF 5 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 6"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) => FromJSON1 ((,,,,,) a b c d e) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f) => FromJSON (a, b, c, d, e, f) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) => FromJSON2 ((,,,,,,) a b c d e) where
    liftParseJSON2 pF _ pG _ = withArray "(a, b, c, d, e, f, g)" $ \t ->
        let n = V.length t
        in if n == 7
            then (,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex pF 5 t
                <*> parseJSONElemAtIndex pG 6 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 7"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f) => FromJSON1 ((,,,,,,) a b c d e f) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g) => FromJSON (a, b, c, d, e, f, g) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f) => FromJSON2 ((,,,,,,,) a b c d e f) where
    liftParseJSON2 pG _ pH _ = withArray "(a, b, c, d, e, f, g, h)" $ \t ->
        let n = V.length t
        in if n == 8
            then (,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex pG 6 t
                <*> parseJSONElemAtIndex pH 7 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 8"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g) => FromJSON1 ((,,,,,,,) a b c d e f g) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h) => FromJSON (a, b, c, d, e, f, g, h) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g) => FromJSON2 ((,,,,,,,,) a b c d e f g) where
    liftParseJSON2 pH _ pI _ = withArray "(a, b, c, d, e, f, g, h, i)" $ \t ->
        let n = V.length t
        in if n == 9
            then (,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex pH 7 t
                <*> parseJSONElemAtIndex pI 8 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 9"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h) => FromJSON1 ((,,,,,,,,) a b c d e f g h) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i) => FromJSON (a, b, c, d, e, f, g, h, i) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h) => FromJSON2 ((,,,,,,,,,) a b c d e f g h) where
    liftParseJSON2 pI _ pJ _ = withArray "(a, b, c, d, e, f, g, h, i, j)" $ \t ->
        let n = V.length t
        in if n == 10
            then (,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex pI 8 t
                <*> parseJSONElemAtIndex pJ 9 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 10"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i) => FromJSON1 ((,,,,,,,,,) a b c d e f g h i) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j) => FromJSON (a, b, c, d, e, f, g, h, i, j) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i) => FromJSON2 ((,,,,,,,,,,) a b c d e f g h i) where
    liftParseJSON2 pJ _ pK _ = withArray "(a, b, c, d, e, f, g, h, i, j, k)" $ \t ->
        let n = V.length t
        in if n == 11
            then (,,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex parseJSON 8 t
                <*> parseJSONElemAtIndex pJ 9 t
                <*> parseJSONElemAtIndex pK 10 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 11"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j) => FromJSON1 ((,,,,,,,,,,) a b c d e f g h i j) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k) => FromJSON (a, b, c, d, e, f, g, h, i, j, k) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j) => FromJSON2 ((,,,,,,,,,,,) a b c d e f g h i j) where
    liftParseJSON2 pK _ pL _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l)" $ \t ->
        let n = V.length t
        in if n == 12
            then (,,,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex parseJSON 8 t
                <*> parseJSONElemAtIndex parseJSON 9 t
                <*> parseJSONElemAtIndex pK 10 t
                <*> parseJSONElemAtIndex pL 11 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 12"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k) => FromJSON1 ((,,,,,,,,,,,) a b c d e f g h i j k) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l) => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k) => FromJSON2 ((,,,,,,,,,,,,) a b c d e f g h i j k) where
    liftParseJSON2 pL _ pM _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m)" $ \t ->
        let n = V.length t
        in if n == 13
            then (,,,,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex parseJSON 8 t
                <*> parseJSONElemAtIndex parseJSON 9 t
                <*> parseJSONElemAtIndex parseJSON 10 t
                <*> parseJSONElemAtIndex pL 11 t
                <*> parseJSONElemAtIndex pM 12 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 13"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l) => FromJSON1 ((,,,,,,,,,,,,) a b c d e f g h i j k l) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m) => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l) => FromJSON2 ((,,,,,,,,,,,,,) a b c d e f g h i j k l) where
    liftParseJSON2 pM _ pN _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m, n)" $ \t ->
        let n = V.length t
        in if n == 14
            then (,,,,,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex parseJSON 8 t
                <*> parseJSONElemAtIndex parseJSON 9 t
                <*> parseJSONElemAtIndex parseJSON 10 t
                <*> parseJSONElemAtIndex parseJSON 11 t
                <*> parseJSONElemAtIndex pM 12 t
                <*> parseJSONElemAtIndex pN 13 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 14"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m) => FromJSON1 ((,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m, FromJSON n) => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m) => FromJSON2 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    liftParseJSON2 pN _ pO _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)" $ \t ->
        let n = V.length t
        in if n == 15
            then (,,,,,,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex parseJSON 8 t
                <*> parseJSONElemAtIndex parseJSON 9 t
                <*> parseJSONElemAtIndex parseJSON 10 t
                <*> parseJSONElemAtIndex parseJSON 11 t
                <*> parseJSONElemAtIndex parseJSON 12 t
                <*> parseJSONElemAtIndex pN 13 t
                <*> parseJSONElemAtIndex pO 14 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 15"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m, FromJSON n) => FromJSON1 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m, FromJSON n, FromJSON o) => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}
