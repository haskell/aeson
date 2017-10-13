{-# LANGUAGE CPP #-}

module Data.Aeson.Compat
  (
    fromStrict
  ) where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

fromStrict :: S.ByteString -> L.ByteString
#if MIN_VERSION_bytestring(0, 9, 2)
fromStrict = L.fromChunks . (:[])
#else
fromStrict = L.fromStrict
#endif
