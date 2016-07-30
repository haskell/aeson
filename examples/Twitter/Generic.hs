-- Use GHC generics to automatically generate good instances.
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Twitter.Generic
    (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where

import Prelude ()
import Prelude.Compat

import Twitter

#ifndef HAS_BOTH_AESON_AND_BENCHMARKS
import Data.Aeson (ToJSON, FromJSON)
#else
import "aeson" Data.Aeson (ToJSON, FromJSON)
import qualified "aeson-benchmarks" Data.Aeson as B
#endif

instance ToJSON Metadata
instance FromJSON Metadata

instance ToJSON Geo
instance FromJSON Geo

instance ToJSON Story
instance FromJSON Story

instance ToJSON Result
instance FromJSON Result

#ifdef HAS_BOTH_AESON_AND_BENCHMARKS
instance B.ToJSON Metadata
instance B.FromJSON Metadata

instance B.ToJSON Geo
instance B.FromJSON Geo

instance B.ToJSON Story
instance B.FromJSON Story

instance B.ToJSON Result
instance B.FromJSON Result
#endif
