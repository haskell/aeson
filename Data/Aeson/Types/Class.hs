{-# LANGUAGE CPP, GADTs, DefaultSignatures, FlexibleInstances, FlexibleContexts, DeriveFunctor,
    ScopedTypeVariables, TypeSynonymInstances #-}

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
    , genericToJSON
    , genericToEncoding
    , genericParseJSON
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

    -- * Functions
    , fromJSON
    , ifromJSON
    , (.:)
    , (.:?)
    , (.:!)
    , (.!=)
    , typeMismatch
    ) where

import Data.Aeson.Types.FromJSON
import Data.Aeson.Types.ToJSON
