{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
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
    , FromArgs(..)
    , GToJSON
    , GToEncoding
    , ToArgs(..)
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
    , toJSONKeyText
    , contramapToJSONKeyFunction
    , FromJSONKey(..)
    , FromJSONKeyFunction(..)
    , fromJSONKeyCoerce
    , coerceFromJSONKeyFunction
    , mapFromJSONKeyFunction
    -- * Object key-value pairs
    , KeyValue(..)

    -- * List functions
    , listEncoding
    , listValue
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
    ) where

import Prelude ()

import Data.Aeson.Types.FromJSON
import Data.Aeson.Types.Generic (One, Zero)
import Data.Aeson.Types.ToJSON hiding (GToJSON)
import qualified Data.Aeson.Types.ToJSON as ToJSON
import Data.Aeson.Types.Internal (Value)
import Data.Aeson.Encoding (Encoding)

type GToJSON = ToJSON.GToJSON Value
type GToEncoding = ToJSON.GToJSON Encoding
