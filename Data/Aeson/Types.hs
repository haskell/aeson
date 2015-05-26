-- |
-- Module:      Data.Aeson.Types
-- Copyright:   (c) 2011, 2012 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
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
    , JSONPath
    , formatError

    -- ** Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , genericToJSON
    , genericParseJSON

    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withNumber
    , withScientific
    , withBool


    , series
    , (.:)
    , (.:?)
    , (.!=)
    , object

    -- * Generic and TH encoding configuration
    , Options(..)
    , SumEncoding(..)
    , camelTo
    , defaultOptions
    , defaultTaggedObject
    ) where

import Data.Aeson.Types.Generic ()
import Data.Aeson.Types.Instances
import Data.Aeson.Types.Internal
