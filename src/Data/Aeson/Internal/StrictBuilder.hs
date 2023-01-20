{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Aeson.Internal.StrictBuilder (
    toStrictByteString,
    toStrictByteStringIO,
) where

import Data.ByteString.Builder.Internal (BufferRange (BufferRange), BuildStep, Builder, fillWithBuildStep, runBuilder)
import Data.ByteString.Internal         (ByteString (..))
import Data.Word                        (Word8)
import GHC.Exts                         (Addr#, Ptr (..), minusAddr#, plusAddr#)
import GHC.Exts                         (Int (I#), Int#, orI#, (+#))
import GHC.Exts                         (MutableByteArray#, RealWorld, newPinnedByteArray#, resizeMutableByteArray#, shrinkMutableByteArray#)
import GHC.ForeignPtr                   (ForeignPtr (ForeignPtr), ForeignPtrContents (PlainPtr))
import GHC.IO                           (IO (IO), unIO, unsafePerformIO)

#if MIN_VERSION_base(4,16,0)
import GHC.Exts (mutableByteArrayContents#)
#else
import GHC.Exts (byteArrayContents#, unsafeCoerce#)

mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# mba = byteArrayContents# (unsafeCoerce# mba)
#endif

toStrictByteString :: Builder -> ByteString
toStrictByteString b = unsafePerformIO (toStrictByteStringIO b)
{-# NOINLINE toStrictByteString #-}

toStrictByteStringIO :: Builder -> IO ByteString
toStrictByteStringIO b = IO $ \s ->
    case newPinnedByteArray# 4096# s of
        (# s', mba #) -> case mutableByteArrayContents# mba of
            start -> unIO (toStrictByteStringWorker mba 4096# start start (plusAddr# start 4096#) (runBuilder b)) s'

-- Progressively double the buffer size if it's reported to be full.
-- (convertion to lazy bytestring allocates new buffer chunks).
toStrictByteStringWorker
    :: MutableByteArray# RealWorld  -- ^ the buffer bytearray
    -> Int#                         -- ^ size of the bytearray
    -> Addr#                        -- ^ beginning of the bytearray
    -> Addr#                        -- ^ current write position
    -> Addr#                        -- ^ end of the bytearray
    -> BuildStep ()
    -> IO ByteString
toStrictByteStringWorker mba size start begin end !curr =
    fillWithBuildStep curr kDone kFull kChunk (BufferRange (Ptr begin) (Ptr end))
  where
    kDone :: Ptr Word8 -> () -> IO ByteString
    kDone (Ptr pos) _ = IO $ \s1 ->
        case minusAddr# pos start                of { len ->
        case shrinkMutableByteArray# mba len s1  of { s2 ->
#if MIN_VERSION_bytestring(0,11,0)
        (# s2 , BS (ForeignPtr start (PlainPtr mba)) (I# len) #)
#else
        (# s2 , PS (ForeignPtr start (PlainPtr mba)) 0 (I# len) #)
#endif
        }}

    kFull :: Ptr Word8 -> Int -> BuildStep () -> IO ByteString
    kFull (Ptr pos) (I# nsize) next = IO $ \s1 ->
        -- orI# is an approximation of max
        case size +# orI# size nsize               of { size' ->
        case resizeMutableByteArray# mba size' s1  of { (# s2, mba' #) ->
        case mutableByteArrayContents# mba'        of { start' ->
        unIO (toStrictByteStringWorker mba' size' start' (plusAddr# start' (minusAddr# pos start)) (plusAddr# start' size') next) s2
        }}}

    kChunk :: Ptr Word8 -> ByteString -> BuildStep () -> IO ByteString
#if MIN_VERSION_bytestring(0,11,0)
    kChunk (Ptr pos) (BS _ 0)   next = toStrictByteStringWorker mba size start pos end next
#else
    kChunk (Ptr pos) (PS _ _ 0) next = toStrictByteStringWorker mba size start pos end next
#endif
    kChunk _         _          _    = fail "TODO: non-empty chunk"
