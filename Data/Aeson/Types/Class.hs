{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module:      Data.Aeson.Types.Class
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Class
    (
    -- * Core JSON classes
      FromJSON(..)
    , ToJSON(..)
    -- * Liftings to unary and binary type constructors
    , FromJSON1(..)
    , parseJSON1
    , FromJSON2(..)
    , parseJSON2
    , ToJSON1(..)
    , toJSON1
    , toEncoding1
    , ToJSON2(..)
    , toJSON2
    , toEncoding2
    -- * Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , GToEncoding(..)
    , Zero
    , One
    , genericToJSON
    , genericLiftToJSON
    , genericToEncoding
    , genericLiftToEncoding
    , genericParseJSON
    , genericLiftParseJSON
    -- * Classes and types for map keys
    , ToJSONKey(..)
    , ToJSONKeyFunction(..)
    , contramapToJSONKeyFunction
    , FromJSONKey(..)
    , FromJSONKeyFunction(..)
    , fromJSONKeyCoerce
    , coerceFromJSONKeyFunction
    , mapFromJSONKeyFunction
    -- * Object key-value pairs
    , KeyValue(..)
    -- * Functions needed for documentation
    , typeMismatch
    -- * Encoding functions
    , listEncoding
    , listValue
    , listParser
    ) where

import Data.Aeson.Types.Internal
import Data.Aeson.Encoding.Internal (Encoding, Series)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Generics (Generic(..), Generic1(..))
import qualified Data.Aeson.Encoding.Internal as E
import qualified Data.Aeson.Encode.Builder as EB
import qualified Data.Vector as V

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif

-- Coercible derivations aren't as powerful on GHC 7.8, though supported.
#define HAS_COERCIBLE (__GLASGOW_HASKELL__ >= 709)

#if HAS_COERCIBLE
import Data.Coerce (Coercible, coerce)
coerce' :: Coercible a b => a -> b
coerce' = coerce
#else
import Unsafe.Coerce (unsafeCoerce)
coerce' :: a -> b
coerce' = unsafeCoerce
#endif

-- | Class of generic representation types that can be converted to
-- JSON.
class GToJSON arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'toJSON' (if the @arity@ is 'Zero')
    -- or 'liftToJSON' (if the @arity@ is 'One').
    gToJSON :: Options -> Proxy arity
            -> (a -> Value) -> ([a] -> Value) -> f a -> Value

-- | Class of generic representation types that can be converted to
-- a JSON 'Encoding'.
class GToEncoding arity f where
    -- | This method (applied to 'defaultOptions') can be used as the
    -- default generic implementation of 'toEncoding' (if the @arity@ is 'Zero')
    -- or 'liftToEncoding' (if the @arity@ is 'One').
    gToEncoding :: Options -> Proxy arity
                -> (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding

-- | Class of generic representation types that can be converted from JSON.
class GFromJSON arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'parseJSON' (if the @arity@ is 'Zero')
    -- or 'liftParseJSON' (if the @arity@ is 'One').
    gParseJSON :: Options -> Proxy arity
               -> (Value -> Parser a) -> (Value -> Parser [a])
               -> Value -> Parser (f a)

-- | A type-level indicator that 'ToJSON' or 'FromJSON' is being derived generically.
data Zero

-- | A type-level indicator that 'ToJSON1' or 'FromJSON1' is being derived generically.
data One

proxyZero :: Proxy Zero
proxyZero = Proxy

proxyOne :: Proxy One
proxyOne  = Proxy

-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'toJSON' when the type
-- is an instance of 'Generic'.
genericToJSON :: (Generic a, GToJSON Zero (Rep a))
              => Options -> a -> Value
genericToJSON opts = gToJSON opts proxyZero undefined undefined . from

-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'liftToJSON' when the type
-- is an instance of 'Generic1'.
genericLiftToJSON :: (Generic1 f, GToJSON One (Rep1 f))
                  => Options -> (a -> Value) -> ([a] -> Value)
                  -> f a -> Value
genericLiftToJSON opts tj tjl = gToJSON opts proxyOne tj tjl . from1

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'toEncoding' when the type
-- is an instance of 'Generic'.
genericToEncoding :: (Generic a, GToEncoding Zero (Rep a))
                  => Options -> a -> Encoding
genericToEncoding opts = gToEncoding opts proxyZero undefined undefined . from

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftToEncoding' when the type
-- is an instance of 'Generic1'.
genericLiftToEncoding :: (Generic1 f, GToEncoding One (Rep1 f))
                      => Options -> (a -> Encoding) -> ([a] -> Encoding)
                      -> f a -> Encoding
genericLiftToEncoding opts te tel = gToEncoding opts proxyOne te tel . from1

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'parseJSON' when the
-- type is an instance of 'Generic'.
genericParseJSON :: (Generic a, GFromJSON Zero (Rep a))
                 => Options -> Value -> Parser a
genericParseJSON opts = fmap to . gParseJSON opts proxyZero undefined undefined

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftParseJSON' when the
-- type is an instance of 'Generic1'.
genericLiftParseJSON :: (Generic1 f, GFromJSON One (Rep1 f))
                     => Options -> (Value -> Parser a) -> (Value -> Parser [a])
                     -> Value -> Parser (f a)
genericLiftParseJSON opts pj pjl = fmap to1 . gParseJSON opts proxyOne pj pjl

-- | A type that can be converted to JSON.
--
-- An example type and instance:
--
-- @
-- \-- Allow ourselves to write 'Text' literals.
-- {-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance ToJSON Coord where
--   toJSON (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y]
--
--   toEncoding (Coord x y) = 'pairs' (\"x\" '.=' x '<>' \"y\" '.=' y)
-- @
--
-- Instead of manually writing your 'ToJSON' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * The compiler can provide a default generic implementation for
-- 'toJSON'.
--
-- To use the second, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a 'ToJSON' instance for your datatype without giving
-- definitions for 'toJSON' or 'toEncoding'.
--
-- For example, the previous example can be simplified to a more
-- minimal instance:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Coord = Coord { x :: Double, y :: Double } deriving 'Generic'
--
-- instance ToJSON Coord where
--     toEncoding = 'genericToEncoding' 'defaultOptions'
-- @
--
-- Why do we provide an implementation for 'toEncoding' here?  The
-- 'toEncoding' function is a relatively new addition to this class.
-- To allow users of older versions of this library to upgrade without
-- having to edit all of their instances or encounter surprising
-- incompatibilities, the default implementation of 'toEncoding' uses
-- 'toJSON'.  This produces correct results, but since it performs an
-- intermediate conversion to a 'Value', it will be less efficient
-- than directly emitting an 'Encoding'.  Our one-liner definition of
-- 'toEncoding' above bypasses the intermediate 'Value'.
--
-- If @DefaultSignatures@ doesn't give exactly the results you want,
-- you can customize the generic encoding with only a tiny amount of
-- effort, using 'genericToJSON' and 'genericToEncoding' with your
-- preferred 'Options':
--
-- @
-- instance ToJSON Coord where
--     toJSON     = 'genericToJSON' 'defaultOptions'
--     toEncoding = 'genericToEncoding' 'defaultOptions'
-- @
class ToJSON a where
    -- | Convert a Haskell value to a JSON-friendly intermediate type.
    toJSON     :: a -> Value

    default toJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
    toJSON = genericToJSON defaultOptions

    -- | Encode a Haskell value as JSON.
    --
    -- The default implementation of this method creates an
    -- intermediate 'Value' using 'toJSON'.  This provides
    -- source-level compatibility for people upgrading from older
    -- versions of this library, but obviously offers no performance
    -- advantage.
    --
    -- To benefit from direct encoding, you /must/ provide an
    -- implementation for this method.  The easiest way to do so is by
    -- having your types implement 'Generic' using the @DeriveGeneric@
    -- extension, and then have GHC generate a method body as follows.
    --
    -- @
    -- instance ToJSON Coord where
    --     toEncoding = 'genericToEncoding' 'defaultOptions'
    -- @

    toEncoding :: a -> Encoding
    toEncoding = E.Encoding . EB.encodeToBuilder . toJSON
    {-# INLINE toEncoding #-}

    toJSONList :: [a] -> Value
    toJSONList = listValue toJSON
    {-# INLINE toJSONList #-}

    toEncodingList :: [a] -> Encoding
    toEncodingList = listEncoding toEncoding
    {-# INLINE toEncodingList #-}

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
-- * 'empty' and 'mzero' work, but are terse and uninformative
--
-- * 'fail' yields a custom error message
--
-- * 'typeMismatch' produces an informative message for cases when the
-- value encountered is not of the expected type
--
-- An example type and instance:
--
-- @
-- \-- Allow ourselves to write 'Text' literals.
-- {-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance FromJSON Coord where
--   parseJSON ('Object' v) = Coord    '<$>'
--                          v '.:' \"x\" '<*>'
--                          v '.:' \"y\"
--
--   \-- We do not expect a non-'Object' value here.
--   \-- We could use 'mzero' to fail, but 'typeMismatch'
--   \-- gives a much more informative error message.
--   parseJSON invalid    = 'typeMismatch' \"Coord\" invalid
-- @
--
-- Instead of manually writing your 'FromJSON' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
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
-- instance FromJSON Coord
-- @
--
-- If @DefaultSignatures@ doesn't give exactly the results you want,
-- you can customize the generic decoding with only a tiny amount of
-- effort, using 'genericParseJSON' with your preferred 'Options':
--
-- @
-- instance FromJSON Coord where
--     parseJSON = 'genericParseJSON' 'defaultOptions'
-- @

class FromJSON a where
    parseJSON :: Value -> Parser a

    default parseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
    parseJSON = genericParseJSON defaultOptions

    parseJSONList :: Value -> Parser [a]
    parseJSONList (Array a)
        = sequence
        . zipWith parseIndexedJSON [0..]
        . V.toList
        $ a
      where
        parseIndexedJSON :: FromJSON a => Int -> Value -> Parser a
        parseIndexedJSON idx value = parseJSON value <?> Index idx

    parseJSONList v = typeMismatch "[a]" v

-------------------------------------------------------------------------------
-- Object key-value pairs
-------------------------------------------------------------------------------

-- | A key-value pair for encoding a JSON object.
class KeyValue kv where
    (.=) :: ToJSON v => Text -> v -> kv
    infixr 8 .=

instance KeyValue Series where
    name .= value = E.Value $ E.text name <> E.colon <> toEncoding value
    {-# INLINE (.=) #-}

instance KeyValue Pair where
    name .= value = (name, toJSON value)
    {-# INLINE (.=) #-}

-------------------------------------------------------------------------------
--  Classes and types for map keys
-------------------------------------------------------------------------------

class ToJSONKey a where
    toJSONKey :: ToJSONKeyFunction a
    default toJSONKey :: ToJSON a => ToJSONKeyFunction a
    toJSONKey = ToJSONKeyValue (toJSON, toEncoding)
    toJSONKeyList :: ToJSONKeyFunction [a]
    default toJSONKeyList :: ToJSON a => ToJSONKeyFunction [a]
    toJSONKeyList = ToJSONKeyValue (toJSON, toEncoding)

class FromJSONKey a where
    fromJSONKey :: FromJSONKeyFunction a
    default fromJSONKey :: FromJSON a => FromJSONKeyFunction a
    fromJSONKey = FromJSONKeyValue parseJSON
    fromJSONKeyList :: FromJSONKeyFunction [a]
    default fromJSONKeyList :: FromJSON a => FromJSONKeyFunction [a]
    fromJSONKeyList = FromJSONKeyValue parseJSON

data ToJSONKeyFunction a
    = ToJSONKeyText (a -> Text, a -> Encoding)
    | ToJSONKeyValue (a -> Value, a -> Encoding)

-- | With GHC 7.8 + we carry around 'Coercible Text a' dictionary,
-- to have even some amount of safety net.
-- Unfortunately we cannot enforce that 'Hashable' instance agree on the type level
--
-- ATM this type is intentionally not exported. FromJSONKeyFunction can be inspected,
-- but cannot be constructed.
data CoerceText a where
#if HAS_COERCIBLE
    CoerceText :: Coercible Text a => CoerceText a
#else
    CoerceText :: CoerceText a
#endif

data FromJSONKeyFunction a
    = FromJSONKeyCoerce (CoerceText a)
    | FromJSONKeyText (Text -> a)
    | FromJSONKeyTextParser (Text -> Parser a)
    | FromJSONKeyValue (Value -> Parser a)

instance Functor FromJSONKeyFunction where
    fmap h (FromJSONKeyCoerce CoerceText) = FromJSONKeyText (h . coerce')
    fmap h (FromJSONKeyText f)            = FromJSONKeyText (h . f)
    fmap h (FromJSONKeyTextParser f)      = FromJSONKeyTextParser (fmap h . f)
    fmap h (FromJSONKeyValue f)           = FromJSONKeyValue (fmap h . f)

-- | Construct 'FromJSONKeyFunction' for types coercible from 'Text'. This
-- conversion is still unsafe, as 'Hashable' and 'Eq' instances of @a@ should be
-- compatible with 'Text' i.e. hash values be equal for wrapped values as well.
fromJSONKeyCoerce ::
#if HAS_COERCIBLE
    Coercible Text a =>
#endif
    FromJSONKeyFunction a
fromJSONKeyCoerce = FromJSONKeyCoerce CoerceText

coerceFromJSONKeyFunction ::
#if HAS_COERCIBLE
    Coercible a b =>
#endif
    FromJSONKeyFunction a -> FromJSONKeyFunction b
coerceFromJSONKeyFunction (FromJSONKeyCoerce CoerceText) = FromJSONKeyCoerce CoerceText
coerceFromJSONKeyFunction (FromJSONKeyText f)            = FromJSONKeyText (coerce' . f)
coerceFromJSONKeyFunction (FromJSONKeyTextParser f)      = FromJSONKeyTextParser (fmap coerce' . f)
coerceFromJSONKeyFunction (FromJSONKeyValue f)           = FromJSONKeyValue (fmap coerce' . f)

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

contramapToJSONKeyFunction :: (b -> a) -> ToJSONKeyFunction a -> ToJSONKeyFunction b
contramapToJSONKeyFunction h x = case x of
    ToJSONKeyText (f,g) -> ToJSONKeyText (f . h, g . h)
    ToJSONKeyValue (f,g) -> ToJSONKeyValue (f . h, g . h)

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
-- so will probably be more efficient than the following two options:
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
-- instance FromJSON a => FromJSON1 (Pair a)
-- @
--
-- If @DefaultSignatures@ doesn't give exactly the results you want,
-- you can customize the generic decoding with only a tiny amount of
-- effort, using 'genericLiftParseJSON' with your preferred 'Options':
--
-- @
-- instance FromJSON a => FromJSON1 (Pair a) where
--     liftParseJSON = 'genericLiftParseJSON' 'defaultOptions'
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

-- | Lifting of the 'ToJSON' class to unary type constructors.
--
-- Instead of manually writing your 'ToJSON1' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * The compiler can provide a default generic implementation for
-- 'toJSON1'.
--
-- To use the second, simply add a @deriving 'Generic1'@ clause to your
-- datatype and declare a 'ToJSON1' instance for your datatype without giving
-- definitions for 'liftToJSON' or 'liftToEncoding'.
--
-- For example:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Pair = Pair { pairFst :: a, pairSnd :: b } deriving 'Generic1'
--
-- instance ToJSON a => ToJSON1 (Pair a)
-- @
--
-- If @DefaultSignatures@ doesn't give exactly the results you want,
-- you can customize the generic encoding with only a tiny amount of
-- effort, using 'genericLiftToJSON' and 'genericLiftToEncoding' with
-- your preferred 'Options':
--
-- @
-- instance ToJSON a => ToJSON1 (Pair a) where
--     liftToJSON     = 'genericLiftToJSON' 'defaultOptions'
--     liftToEncoding = 'genericLiftToEncoding' 'defaultOptions'
-- @
class ToJSON1 f where
    liftToJSON :: (a -> Value) -> ([a] -> Value) -> f a -> Value

    default liftToJSON :: (Generic1 f, GToJSON One (Rep1 f))
                       => (a -> Value) -> ([a] -> Value) -> f a -> Value
    liftToJSON = genericLiftToJSON defaultOptions

    liftToJSONList :: (a -> Value) -> ([a] -> Value) -> [f a] -> Value
    liftToJSONList f g = listValue (liftToJSON f g)

    liftToEncoding :: (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding

    default liftToEncoding :: (Generic1 f, GToEncoding One (Rep1 f))
                           => (a -> Encoding) -> ([a] -> Encoding)
                           -> f a -> Encoding
    liftToEncoding = genericLiftToEncoding defaultOptions

    liftToEncodingList :: (a -> Encoding) -> ([a] -> Encoding) -> [f a] -> Encoding
    liftToEncodingList f g = listEncoding (liftToEncoding f g)

-- | Lift the standard 'toJSON' function through the type constructor.
toJSON1 :: (ToJSON1 f, ToJSON a) => f a -> Value
toJSON1 = liftToJSON toJSON toJSONList
{-# INLINE toJSON1 #-}

-- | Lift the standard 'toEncoding' function through the type constructor.
toEncoding1 :: (ToJSON1 f, ToJSON a) => f a -> Encoding
toEncoding1 = liftToEncoding toEncoding toEncodingList
{-# INLINE toEncoding1 #-}


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

-- | Lifting of the 'ToJSON' class to binary type constructors.
--
-- Instead of manually writing your 'ToJSON2' instance, "Data.Aeson.TH"
-- provides Template Haskell functions which will derive an instance at compile time.
--
-- The compiler cannot provide a default generic implementation for 'liftToJSON2',
-- unlike 'toJSON' and 'liftToJSON'.
class ToJSON2 f where
    liftToJSON2 :: (a -> Value) -> ([a] -> Value) -> (b -> Value) -> ([b] -> Value) -> f a b -> Value
    liftToJSONList2 :: (a -> Value) -> ([a] -> Value) -> (b -> Value) -> ([b] -> Value) -> [f a b] -> Value
    liftToJSONList2 fa ga fb gb = listValue (liftToJSON2 fa ga fb gb)

    liftToEncoding2 :: (a -> Encoding) -> ([a] -> Encoding) -> (b -> Encoding) -> ([b] -> Encoding) -> f a b -> Encoding
    liftToEncodingList2 :: (a -> Encoding) -> ([a] -> Encoding) -> (b -> Encoding) -> ([b] -> Encoding) -> [f a b] -> Encoding
    liftToEncodingList2 fa ga fb gb = listEncoding (liftToEncoding2 fa ga fb gb)

-- | Lift the standard 'toJSON' function through the type constructor.
toJSON2 :: (ToJSON2 f, ToJSON a, ToJSON b) => f a b -> Value
toJSON2 = liftToJSON2 toJSON toJSONList toJSON toJSONList
{-# INLINE toJSON2 #-}

-- | Lift the standard 'toEncoding' function through the type constructor.
toEncoding2 :: (ToJSON2 f, ToJSON a, ToJSON b) => f a b -> Encoding
toEncoding2 = liftToEncoding2 toEncoding toEncodingList toEncoding toEncodingList
{-# INLINE toEncoding2 #-}

-------------------------------------------------------------------------------
-- Encoding functions
-------------------------------------------------------------------------------

listEncoding :: (a -> Encoding) -> [a] -> Encoding
listEncoding = E.list

listValue :: (a -> Value) -> [a] -> Value
listValue f = Array . V.fromList . map f
{-# INLINE listValue #-}

listParser :: (Value -> Parser a) -> Value -> Parser [a]
listParser f (Array xs) = fmap V.toList (V.mapM f xs)
listParser _ v          = typeMismatch "[a]" v
{-# INLINE listParser #-}

-------------------------------------------------------------------------------
-- [] instances
-------------------------------------------------------------------------------

-- These are needed for key-class default definitions

instance ToJSON1 [] where
    liftToJSON _ to' = to'
    {-# INLINE liftToJSON #-}

    liftToEncoding _ to' = to'
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON [a] where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 [] where
    liftParseJSON _ p' = p'
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON [a] where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}
