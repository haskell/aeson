-- Use GHC generics to automatically generate good instances.

{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import Data.Aeson (ToJSON (..), FromJSON (..), genericToJSON, genericToEncoding, genericParseJSON)

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
