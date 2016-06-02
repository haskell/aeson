{-# LANGUAGE CPP #-}

module Data.Aeson.Encode.Functions
    (
      builder
    , char7 -- TODO: used by TH module
    , encode
    ) where

import Data.Aeson.Encoding
import Data.Aeson.Types.Class
import Data.ByteString.Builder (Builder, char7)
import Data.Monoid ((<>))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mempty)
#endif

builder :: ToJSON a => a -> Builder
builder = fromEncoding . toEncoding
{-# INLINE builder #-}

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
--
-- This is implemented in terms of the 'ToJSON' class's 'toEncoding' method.
encode :: ToJSON a => a -> L.ByteString
encode = B.toLazyByteString . builder
{-# INLINE encode #-}
