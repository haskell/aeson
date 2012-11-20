-- |
-- Module:      Data.Aeson
-- Copyright:   (c) 2011 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working efficiently with JSON data.
--
-- (A note on naming: in Greek mythology, Aeson was the father of Jason.)

module Data.Aeson
    (
    -- * Encoding and decoding
      decode
    , decode'
    , eitherDecode
    , eitherDecode'
    , encode
    -- * Core JSON types
    , Value(..)
    , Array
    , Object
    -- * Convenience types
    , DotNetTime(..)
    -- * Type conversion
    , FromJSON(..)
    , Result(..)
    , fromJSON
    , ToJSON(..)
    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withNumber
    , withBool
    -- * Constructors and accessors
    , (.=)
    , (.:)
    , (.:?)
    , (.!=)
    , object
    -- * Parsing
    , json
    , json'
    ) where

import Data.Aeson.Encode (encode)
import Data.Aeson.Parser.Internal (decodeWith, eitherDecodeWith, json, json')
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
decode :: (FromJSON a) => L.ByteString -> Maybe a
decode = decodeWith json fromJSON
{-# INLINE decode #-}

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- This function parses and performs conversion immediately.  See
-- 'json'' for details.
decode' :: (FromJSON a) => L.ByteString -> Maybe a
decode' = decodeWith json' fromJSON
{-# INLINE decode' #-}

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode = eitherDecodeWith json fromJSON
{-# INLINE eitherDecode #-}

-- | Like 'decode'' but returns an error message when decoding fails.
eitherDecode' :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode' = eitherDecodeWith json' fromJSON
{-# INLINE eitherDecode' #-}
