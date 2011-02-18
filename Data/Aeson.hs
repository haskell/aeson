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
    , (.=?)
    , (.:)
    , (.:?)
    , (./)
    , object
    -- * Encoding and parsing
    , encode
    , json
    ) where

import Data.Aeson.Encode
import Data.Aeson.Parser
import Data.Aeson.Types
