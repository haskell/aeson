{-# LANGUAGE CPP #-}

module Data.Aeson.Encode.Functions
    (
      builder
    , char7 -- TODO: used by TH module
    ) where

import Data.Aeson.Encoding
import Data.Aeson.Types.Class
import Data.ByteString.Builder (Builder, char7)
import Data.Monoid ((<>))

builder :: ToJSON a => a -> Builder
builder = fromEncoding . toEncoding
{-# INLINE builder #-}
