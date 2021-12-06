{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
module Data.Aeson.Internal.Text (
    unsafeDecodeASCII,
) where

import           Data.ByteString                (ByteString)
import qualified Data.Text                      as T

#if MIN_VERSION_text(2,0,0)
import           Data.Text.Array                (Array (..))
import qualified Data.Text.Internal             as T (Text (..))

import qualified Data.ByteString.Short.Internal as SBS

import           Data.Aeson.Internal.ByteString

#else
import qualified Data.Text.Encoding             as TE

#endif

-- | The input is assumed to contain only 7bit ASCII characters (i.e. @< 0x80@).
--   We use TE.decodeLatin1 here because TE.decodeASCII is currently (text-1.2.4.0)
--   deprecated and equal to TE.decodeUtf8, which is slower than TE.decodeLatin1.
unsafeDecodeASCII :: ByteString -> T.Text

#if MIN_VERSION_text(2,0,0)
unsafeDecodeASCII bs = withBS bs $ \_fp len -> if len == 0 then T.empty else
  let !(SBS.SBS arr) = SBS.toShort bs in T.Text (ByteArray arr) 0 len

#else
unsafeDecodeASCII = TE.decodeLatin1
#endif
