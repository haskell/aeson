-- Use GHC generics to automatically generate good instances.

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifdef HAS_BOTH_AESON_AND_BENCHMARKS
{-# LANGUAGE PackageImports #-}
#endif

module Twitter.Generic
    (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where

import Prelude ()
import Prelude.Compat ()

import Twitter
import Twitter.Options

#ifndef HAS_BOTH_AESON_AND_BENCHMARKS
import Data.Aeson (ToJSON (..), FromJSON (..), genericToJSON, genericToEncoding, genericParseJSON)
#else
import "aeson" Data.Aeson (ToJSON (..), FromJSON (..), genericToJSON, genericToEncoding, genericParseJSON)
import qualified "aeson-benchmarks" Data.Aeson as B
#endif

instance ToJSON Metadata where
    toJSON = genericToJSON twitterOptions
    toEncoding = genericToEncoding twitterOptions
instance FromJSON Metadata where
    parseJSON = genericParseJSON twitterOptions

instance ToJSON Geo where
    toJSON = genericToJSON twitterOptions
    toEncoding = genericToEncoding twitterOptions
instance FromJSON Geo where
    parseJSON = genericParseJSON twitterOptions

instance ToJSON Story where
    toJSON = genericToJSON twitterOptions
    toEncoding = genericToEncoding twitterOptions
instance FromJSON Story where
    parseJSON = genericParseJSON twitterOptions

instance ToJSON Result where
    toJSON = genericToJSON twitterOptions
    toEncoding = genericToEncoding twitterOptions
instance FromJSON Result where
    parseJSON = genericParseJSON twitterOptions

#ifdef HAS_BOTH_AESON_AND_BENCHMARKS
instance B.ToJSON Metadata where
    toJSON = B.genericToJSON btwitterOptions
    toEncoding = B.genericToEncoding btwitterOptions
instance B.FromJSON Metadata where
    parseJSON = B.genericParseJSON btwitterOptions

instance B.ToJSON Geo where
    toJSON = B.genericToJSON btwitterOptions
    toEncoding = B.genericToEncoding btwitterOptions
instance B.FromJSON Geo where
    parseJSON = B.genericParseJSON btwitterOptions

instance B.ToJSON Story where
    toJSON = B.genericToJSON btwitterOptions
    toEncoding = B.genericToEncoding btwitterOptions
instance B.FromJSON Story where
    parseJSON = B.genericParseJSON btwitterOptions

instance B.ToJSON Result where
    toJSON = B.genericToJSON btwitterOptions
    toEncoding = B.genericToEncoding btwitterOptions
instance B.FromJSON Result where
    parseJSON = B.genericParseJSON btwitterOptions
#endif
