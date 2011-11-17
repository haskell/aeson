-- |
-- Module:      Data.Aeson
-- Copyright:   (c) 2011 MailRank, Inc.
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
    -- * Constructors and accessors
    , (.=)
    , (.:)
    , (.:?)
    , object
    -- * Parsing
    , json
    ) where

import Data.Aeson.Encode (encode)
import Data.Aeson.Parser (json)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.Lazy as L

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
decode :: (FromJSON a) => L.ByteString -> Maybe a
decode s = case L.parse json s of
             L.Done _ v -> case fromJSON v of
                             Success a -> Just a
                             _         -> Nothing
             _          -> Nothing
{-# INLINE decode #-}
