{-# LANGUAGE CPP #-}

module Data.Aeson.Encode.Functions
    (
      builder
    ) where

import Data.Aeson.Encoding
import Data.Aeson.Types.Class
import Data.ByteString.Builder (Builder)
import Data.Monoid ((<>))


{-# INLINE builder #-}
