-- Use GHC generics to automatically generate good instances.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Twitter.Generic
    (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where

import Data.Aeson (ToJSON, FromJSON)
import Twitter

instance ToJSON Metadata
instance FromJSON Metadata

instance ToJSON Geo
instance FromJSON Geo

instance ToJSON Story
instance FromJSON Story

instance ToJSON Result
instance FromJSON Result
