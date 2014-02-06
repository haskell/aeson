{-# LANGUAGE CPP, FlexibleContexts #-}

#ifdef GENERICS
{-# LANGUAGE DefaultSignatures #-}
#endif

-- |
-- Module:      Data.Aeson.Types.Class
-- Copyright:   (c) 2011-2013 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Class
    (
    -- * Type classes
    -- ** Core JSON classes
      FromJSON(..)
    , ToJSON(..)
#ifdef GENERICS
    -- ** Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , genericToJSON
    , genericParseJSON
#endif
    ) where

import Data.Aeson.Types.Internal

#ifdef GENERICS
import GHC.Generics

-- | Class of generic representation types ('Rep') that can be converted to JSON.
class GToJSON f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'toJSON'.
    gToJSON :: Options -> f a -> Value

-- | Class of generic representation types ('Rep') that can be converted from JSON.
class GFromJSON f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'parseJSON'.
    gParseJSON :: Options -> Value -> Parser (f a)

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'toJSON' when the type
-- is an instance of 'Generic'.
genericToJSON :: (Generic a, GToJSON (Rep a)) => Options -> a -> Value
genericToJSON opts = gToJSON opts . from

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'parseJSON' when the
-- type is an instance of 'Generic'.
genericParseJSON :: (Generic a, GFromJSON (Rep a)) => Options -> Value -> Parser a
genericParseJSON opts = fmap to . gParseJSON opts
#endif

-- | A type that can be converted to JSON.
--
-- An example type and instance:
--
-- @{-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance ToJSON Coord where
--   toJSON (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y]
-- @
--
-- Note the use of the @OverloadedStrings@ language extension which enables
-- 'Text' values to be written as string literals.
--
-- Instead of manually writing your 'ToJSON' instance, there are three options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides template-haskell functions which will derive an
-- instance at compile-time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * "Data.Aeson.Generic" provides a generic @toJSON@ function that accepts any
-- type which is an instance of 'Data'.
--
-- * If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (GHC 7.2 and newer),
-- @toJSON@ will have a default generic implementation.
--
-- To use the latter option, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a @ToJSON@ instance for your datatype without giving a
-- definition for @toJSON@.
--
-- For example the previous example can be simplified to just:
--
-- @{-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics
--
-- data Coord = Coord { x :: Double, y :: Double } deriving Generic
--
-- instance ToJSON Coord
-- @
--
-- Note that, instead of using @DefaultSignatures@, it's also possible
-- to parameterize the generic encoding using 'genericToJSON' applied
-- to your encoding/decoding 'Options':
--
-- @
-- instance ToJSON Coord where
--     toJSON = 'genericToJSON' 'defaultOptions'
-- @
class ToJSON a where
    toJSON   :: a -> Value

#ifdef GENERICS
    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
    toJSON = genericToJSON defaultOptions
#endif

-- | A type that can be converted from JSON, with the possibility of
-- failure.
--
-- When writing an instance, use 'empty', 'mzero', or 'fail' to make a
-- conversion fail, e.g. if an 'Object' is missing a required key, or
-- the value is of the wrong type.
--
-- An example type and instance:
--
-- @{-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance FromJSON Coord where
--   parseJSON ('Object' v) = Coord    '<$>'
--                          v '.:' \"x\" '<*>'
--                          v '.:' \"y\"
--
--   \-- A non-'Object' value is of the wrong type, so use 'mzero' to fail.
--   parseJSON _          = 'mzero'
-- @
--
-- Note the use of the @OverloadedStrings@ language extension which enables
-- 'Text' values to be written as string literals.
--
-- Instead of manually writing your 'FromJSON' instance, there are three options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides template-haskell functions which will derive an
-- instance at compile-time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * "Data.Aeson.Generic" provides a generic @fromJSON@ function that parses to
-- any type which is an instance of 'Data'.
--
-- * If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions, @parseJSON@ will have a default
-- generic implementation.
--
-- To use this, simply add a @deriving 'Generic'@ clause to your datatype and
-- declare a @FromJSON@ instance for your datatype without giving a definition
-- for @parseJSON@.
--
-- For example the previous example can be simplified to just:
--
-- @{-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics
--
-- data Coord = Coord { x :: Double, y :: Double } deriving Generic
--
-- instance FromJSON Coord
-- @
--
-- Note that, instead of using @DefaultSignatures@, it's also possible
-- to parameterize the generic decoding using 'genericParseJSON' applied
-- to your encoding/decoding 'Options':
--
-- @
-- instance FromJSON Coord where
--     parseJSON = 'genericParseJSON' 'defaultOptions'
-- @

class FromJSON a where
    parseJSON :: Value -> Parser a

#ifdef GENERICS
    default parseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
    parseJSON = genericParseJSON defaultOptions
#endif
