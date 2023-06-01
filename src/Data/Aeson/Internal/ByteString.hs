{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Data.Aeson.Internal.ByteString (
    mkBS, 
    withBS,
    liftSBS,
) where

import Data.ByteString.Internal (ByteString (..))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import Data.ByteString.Short (ShortByteString, fromShort)
import GHC.Exts (Addr#, Ptr (Ptr))
import Data.ByteString.Short.Internal (createFromPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH

#if !MIN_VERSION_bytestring(0,11,0)
import GHC.ForeignPtr (plusForeignPtr)
#endif

#if !MIN_VERSION_template_haskell(2,16,0)
import qualified Data.ByteString as BS
#endif

mkBS :: ForeignPtr Word8 -> Int -> ByteString
#if MIN_VERSION_bytestring(0,11,0)
mkBS dfp n = BS dfp n
#else
mkBS dfp n = PS dfp 0 n
#endif
{-# INLINE mkBS #-}

withBS :: ByteString -> (ForeignPtr Word8 -> Int -> r) -> r
#if MIN_VERSION_bytestring(0,11,0)
withBS (BS !sfp !slen)       kont = kont sfp slen
#else
withBS (PS !sfp !soff !slen) kont = kont (plusForeignPtr sfp soff) slen
#endif
{-# INLINE withBS #-}

liftSBS :: ShortByteString -> TH.ExpQ
#if MIN_VERSION_template_haskell(2,16,0)
liftSBS sbs = withBS bs $ \ptr len -> [| unsafePackLenLiteral |]
    `TH.appE` TH.litE (TH.integerL (fromIntegral len))
    `TH.appE` TH.litE (TH.BytesPrimL $ TH.Bytes ptr 0 (fromIntegral len))
    where
      bs = fromShort sbs
#else
liftSBS sbs = withBS bs $ \_ len -> [| unsafePackLenLiteral |]
    `TH.appE` TH.litE (TH.integerL (fromIntegral len))
    `TH.appE` TH.litE (TH.StringPrimL $ BS.unpack bs)
    where
      bs = fromShort sbs
#endif

-- this is copied verbatim from @bytestring@, but only in recent versions.
unsafePackLenLiteral :: Int -> Addr# -> ShortByteString
unsafePackLenLiteral len addr# =
    unsafeDupablePerformIO $ createFromPtr (Ptr addr#) len
