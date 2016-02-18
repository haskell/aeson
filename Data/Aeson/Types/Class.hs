{-# LANGUAGE CPP, DefaultSignatures, FlexibleContexts, FunctionalDependencies, GADTs #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, DataKinds, KindSignatures, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
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
    -- * Map classes
    , FromJSONKey(..)
    , FromJSONKeyType
    , ToJSONKey(..)
    , ToJSONKeyType
    , JSONKeyCoercible
    , JSONKeyMethod(..)
    , SJSONKeyMethod(..)
    , IJSONKeyMethod(..)
    -- * Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , GToEncoding(..)
    , genericToJSON
    , genericToEncoding
    , genericParseJSON
    -- * Object key-value pairs
    , KeyValue(..)
    -- * Functions needed for documentation
    , typeMismatch
    ) where

import Data.Aeson.Types.Internal
import Data.Text (Text)
import GHC.Generics (Generic, Rep, from, to)
import qualified Data.Aeson.Encode.Builder as E

import GHC.Exts (Constraint)
#ifdef HAS_COERCIBLE
import Data.Coerce (Coercible, coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

-- | Class of generic representation types ('Rep') that can be converted to
-- JSON.
class GToJSON f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'toJSON'.
    gToJSON :: Options -> f a -> Value

-- | Class of generic representation types ('Rep') that can be converted to
-- a JSON 'Encoding'.
class GToEncoding f where
    -- | This method (applied to 'defaultOptions') can be used as the
    -- default generic implementation of 'toEncoding'.
    gToEncoding :: Options -> f a -> Encoding

-- | Class of generic representation types ('Rep') that can be converted from JSON.
class GFromJSON f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'parseJSON'.
    gParseJSON :: Options -> Value -> Parser (f a)

-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'toJSON' when the type
-- is an instance of 'Generic'.
genericToJSON :: (Generic a, GToJSON (Rep a)) => Options -> a -> Value
genericToJSON opts = gToJSON opts . from

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'toEncoding' when the type
-- is an instance of 'Generic'.
genericToEncoding :: (Generic a, GToEncoding (Rep a)) => Options -> a -> Encoding
genericToEncoding opts = gToEncoding opts . from

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'parseJSON' when the
-- type is an instance of 'Generic'.
genericParseJSON :: (Generic a, GFromJSON (Rep a)) => Options -> Value -> Parser a
genericParseJSON opts = fmap to . gParseJSON opts

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

    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
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
    toEncoding = Encoding . E.encodeToBuilder . toJSON
    {-# INLINE toEncoding #-}

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

    default parseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
    parseJSON = genericParseJSON defaultOptions

-- | Type family to reduce errors with 'JSONKeyCoerce'. Useful only with GHC >= 7.8
#ifdef HAS_COERCIBLE
type family JSONKeyCoercible (m :: JSONKeyMethod) a :: Constraint where
    JSONKeyCoercible 'JSONKeyCoerce a = (Coercible Text a, Coercible a Text) -- Symmetry for GHC 7.8
    JSONKeyCoercible m              a = ()
#else
type family JSONKeyCoercible (m :: JSONKeyMethod) a :: Constraint
type instance JSONKeyCoercible m a = ()
#endif

-- | Helper typeclass to implement 'FromJSON' for map-like structures.
--
-- 'JSONKeyMethod' provides different method to parse the key. There are three methods to parse textual keys:
--
-- * 'JSONKeyCoerce' for newtypes over 'Text' (with agreeing 'Hashable').
--
-- * 'JSONKeyIdentity' for values which can be always parsed from 'Text', e.g. 'CI' 'Text'.
--
-- * 'JSONKeyTextParser' for other textual values.
--
-- For types without textual representation use 'JSONKeyValueParser':
--
-- @
-- instance FromJSONKey Coord 'JSONKeyValueParser where
--    fromJSONKey _ = parseJSON
-- @
class JSONKeyCoercible m a => FromJSONKey a (m :: JSONKeyMethod) | a -> m where
    fromJSONKey :: proxy m -> FromJSONKeyType m a

-- | Helper typeclass to implement 'ToJSON' for map-like structures. See 'FromJSONKey'.
--
-- For types without textual representation use 'JSONKeyValueParser':
--
-- @
-- instance ToJSONKey Coord 'JSONKeyValueParser where
--    toJSONKey     _ = toJSON
--    toKeyEncoding _ = toEncoding
-- @
class JSONKeyCoercible m a => ToJSONKey a (m :: JSONKeyMethod) | a -> m  where
    toJSONKey :: proxy m -> ToJSONKeyType m a

    -- | For 'JSONKeyValueParser' should produce valid 'Value' encoding.
    --
    -- For other methods 'toKeyEncoding' should produce valid 'Text' encoding.
    toKeyEncoding :: proxy m -> a -> Encoding
    default toKeyEncoding :: DefaultToKeyEncoding m a => proxy m -> a -> Encoding
    toKeyEncoding = defaultToKeyEncoding
    -- {-# INLINE toKeyEncoding #-}

class DefaultToKeyEncoding (m :: JSONKeyMethod) a where
    defaultToKeyEncoding :: proxy m -> a -> Encoding

instance JSONKeyCoercible 'JSONKeyCoerce a => DefaultToKeyEncoding 'JSONKeyCoerce a where
#ifdef HAS_COERCIBLE
    defaultToKeyEncoding _ = Encoding . E.text . coerce
#else
    defaultToKeyEncoding _ = Encoding . E.text . unsafeCoerce
#endif

instance ToJSONKey a 'JSONKeyIdentity => DefaultToKeyEncoding 'JSONKeyIdentity a where
    defaultToKeyEncoding p = Encoding . E.text . toJSONKey p

instance ToJSONKey a 'JSONKeyTextParser => DefaultToKeyEncoding 'JSONKeyTextParser a where
    defaultToKeyEncoding p = Encoding . E.text . toJSONKey p

instance ToJSONKey a 'JSONKeyValueParser => DefaultToKeyEncoding 'JSONKeyValueParser a where
    defaultToKeyEncoding p = Encoding . E.encodeToBuilder . toJSONKey p

-- | Different methods to handle map structure keys
data JSONKeyMethod = JSONKeyCoerce      -- ^ /Unsafe:/ For keys which are newtypes and 'Hashable' instances agree with base type.
                   | JSONKeyIdentity    -- ^ Key parsers which cannot fail.
                   | JSONKeyTextParser  -- ^ Arbitrary key parsers.
                   | JSONKeyValueParser -- ^ Maps serialised as list of key-value pairs.
    deriving (Eq, Ord, Enum, Bounded)

-- | Type of 'fromJSONKey'.
type family FromJSONKeyType (m :: JSONKeyMethod) a
type instance FromJSONKeyType 'JSONKeyCoerce      a = ()
type instance FromJSONKeyType 'JSONKeyIdentity    a = Text -> a
type instance FromJSONKeyType 'JSONKeyTextParser  a = Text -> Parser a
type instance FromJSONKeyType 'JSONKeyValueParser a = Value -> Parser a

-- | Type of 'toJSONKey'.
type family ToJSONKeyType (m :: JSONKeyMethod) a
type instance ToJSONKeyType 'JSONKeyCoerce      a = ()
type instance ToJSONKeyType 'JSONKeyIdentity    a = a -> Text
type instance ToJSONKeyType 'JSONKeyTextParser  a = a -> Text
type instance ToJSONKeyType 'JSONKeyValueParser a = a -> Value

-- | Singleton of 'JSONKeyMethod'.
data SJSONKeyMethod (m :: JSONKeyMethod) where
    SJSONKeyCoerce       :: SJSONKeyMethod 'JSONKeyCoerce
    SJSONKeyIdentity     :: SJSONKeyMethod 'JSONKeyIdentity
    SJSONKeyTextParser   :: SJSONKeyMethod 'JSONKeyTextParser
    SJSONKeyValueParser  :: SJSONKeyMethod 'JSONKeyValueParser

-- | A class for providing 'SJSONKeyMethod' values.
class IJSONKeyMethod (m :: JSONKeyMethod) where
    jsonKeyMethodSing :: proxy m -> SJSONKeyMethod m

instance IJSONKeyMethod 'JSONKeyCoerce where
    jsonKeyMethodSing _ = SJSONKeyCoerce

instance IJSONKeyMethod 'JSONKeyIdentity where
    jsonKeyMethodSing _ = SJSONKeyIdentity

instance IJSONKeyMethod 'JSONKeyTextParser where
    jsonKeyMethodSing _ = SJSONKeyTextParser

instance IJSONKeyMethod 'JSONKeyValueParser where
    jsonKeyMethodSing _ = SJSONKeyValueParser

-- | A key-value pair for encoding a JSON object.
class KeyValue kv where
    (.=) :: ToJSON v => Text -> v -> kv
    infixr 8 .=

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
