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
import Data.ByteString.Internal (accursedUnutterablePerformIO)
import Data.ByteString.Short.Internal (createFromPtr)

import qualified Data.ByteString as BS
import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH

#if !MIN_VERSION_bytestring(0,11,0)
#if MIN_VERSION_base(4,10,0)
import GHC.ForeignPtr (plusForeignPtr)
#else
import GHC.ForeignPtr (ForeignPtr(ForeignPtr))
import GHC.Types (Int (..))
import GHC.Prim (plusAddr#)
#endif
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

#if !MIN_VERSION_bytestring(0,11,0)
#if !MIN_VERSION_base(4,10,0)
-- |Advances the given address by the given offset in bytes.
--
-- The new 'ForeignPtr' shares the finalizer of the original,
-- equivalent from a finalization standpoint to just creating another
-- reference to the original. That is, the finalizer will not be
-- called before the new 'ForeignPtr' is unreachable, nor will it be
-- called an additional time due to this call, and the finalizer will
-- be called with the same address that it would have had this call
-- not happened, *not* the new address.
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr guts) (I# offset) = ForeignPtr (plusAddr# addr offset) guts
{-# INLINE [0] plusForeignPtr #-}
{-# RULES
"ByteString plusForeignPtr/0" forall fp .
   plusForeignPtr fp 0 = fp
 #-}
#endif
#endif

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

unsafePackLenLiteral :: Int -> Addr# -> ShortByteString
unsafePackLenLiteral len addr# =
    accursedUnutterablePerformIO $ createFromPtr (Ptr addr#) len
