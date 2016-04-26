-- |
-- Module:      Data.Aeson.Types
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types
    (
    -- * Core JSON types
      Value(..)
    , Encoding
    , unsafeToEncoding
    , fromEncoding
    , Series
    , Array
    , emptyArray
    , Pair
    , Object
    , emptyObject
    -- * Convenience types and functions
    , DotNetTime(..)
    , typeMismatch
    -- * Type conversion
    , Parser
    , Result(..)
    , FromJSON(..)
    , fromJSON
    , parse
    , parseEither
    , parseMaybe
    , ToJSON(..)
    , KeyValue(..)
    , modifyFailure

    -- ** Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , GToEncoding(..)
    , genericToJSON
    , genericToEncoding
    , genericParseJSON

    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withNumber
    , withScientific
    , withBool

    , pairs
    , foldable
    , (.:)
    , (.:?)
    , (.:!)
    , (.!=)
    , object

    -- * Generic and TH encoding configuration
    , Options(..)
    , SumEncoding(..)
    , camelTo
    , camelTo2
    , defaultOptions
    , defaultTaggedObject
    ) where

import Data.Aeson.Encode.Functions (foldable, pairs)
import Data.Aeson.Types.Generic ()
import Data.Aeson.Types.Instances
import Data.Aeson.Types.Internal
