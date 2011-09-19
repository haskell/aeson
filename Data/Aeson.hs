-- |
-- Module:      Data.Aeson
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working efficiently with JSON data.
--
-- (A note on naming: in Greek mythology, Aeson was the father of Jason.)

module Data.Aeson
    (
    -- * Core JSON types
      Value(..)
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
    -- * Encoding and decoding
    , decode
    , encode
    -- * Parsing
    , json
    ) where

import Data.Aeson.Encode
import Data.Aeson.Parser
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.Lazy as L

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
decode :: (FromJSON a) => L.ByteString -> Maybe a
decode s = case L.parse json s of
             L.Done _ v -> case fromJSON v of
                             Success a -> Just a
                             _         -> Nothing
             _          -> Nothing
{-# INLINE decode #-}
