{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MultiWayIf   #-}

module Data.Aeson.Internal.UnescapeFromText
  ( unescapeFromText
  ) where

import           Control.Exception           (throwIO, try)
import           Data.Bits                   (shiftL, shiftR, (.&.), (.|.))
import           Data.Text.Encoding.Error    (UnicodeException (..))
import           Data.Text.Internal          (Text (..))
import           Data.Text.Unsafe            (unsafeDupablePerformIO)

import           Data.Aeson.Internal.Prelude

import qualified Data.Primitive              as P
import qualified Data.Text.Array             as A
import qualified Data.Text.Internal          as T

#if !MIN_VERSION_text(2,0,0)
import           Data.Word                   (Word16)
#endif

-- | Unescape JSON text literal.
--
-- This function is exporeted mostly for testing and benchmarking purposes.
unescapeFromText :: Text -> Either UnicodeException Text
unescapeFromText = unsafeDupablePerformIO . try . unescapeFromTextIO

throwDecodeError :: IO a
throwDecodeError =
  let desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"
   in throwIO (DecodeError desc Nothing)

-------------------------------------------------------------------------------
-- unescapeTextIO
-------------------------------------------------------------------------------

type Offset = Int

unescapeFromTextIO :: Text -> IO Text

#if MIN_VERSION_text(2,0,0)

unescapeFromTextIO (Text src begin len) = do
    let end :: Offset
        end = begin + len

    arr <- P.newPrimArray len

    let write3bytes :: Int -> Word8 -> Word8 -> Word8 -> Offset -> IO Text
        write3bytes !out !b1 !b2 !b3 !inp = do
          P.writePrimArray arr out b1
          write2bytes (out + 1) b2 b3 inp

        write2bytes :: Int -> Word8 -> Word8 -> Offset -> IO Text
        write2bytes !out !b1 !b2 !inp = do
          P.writePrimArray arr out b1
          write1byte (out + 1) b2 inp

        write1byte :: Int -> Word8 -> Offset -> IO Text
        write1byte !out !b1 !inp = do
          P.writePrimArray arr out b1
          state_start (out + 1) inp

        writeCodePoint :: Int -> Offset -> Word32 -> IO Text
        writeCodePoint !out !inp !acc
          | acc <= 127 = do
            P.writePrimArray arr out (fromIntegral acc :: Word8)
            state_start (out + 1) (inp + 1)

          | acc <= 2047 = do
            let b1 = fromIntegral (shiftR acc 6 .|. 192) :: Word8
            let b2 = fromIntegral ((acc .&. 63) .|. 128) :: Word8
            P.writePrimArray arr out b1
            write1byte (out + 1) b2 (inp + 1)

          | acc <= 65535 = do
            let b1 = fromIntegral (shiftR acc 12 .|. 224) :: Word8
            let b2 = fromIntegral ((shiftR acc 6 .&. 63) .|.  128) :: Word8
            let b3 = fromIntegral ((acc .&. 63) .|. 128) :: Word8
            P.writePrimArray arr out b1
            write2bytes (out + 1) b2 b3 (inp + 1)

          | otherwise = do
            let b1 = fromIntegral (shiftR acc 18 .|. 240) :: Word8
            let b2 = fromIntegral ((shiftR acc 12 .&. 63) .|. 128) :: Word8
            let b3 = fromIntegral ((shiftR acc 6 .&. 63) .|. 128) :: Word8
            let b4 = fromIntegral ((acc .&. 63) .|. 128) :: Word8
            P.writePrimArray arr out b1
            write3bytes (out + 1) b2 b3 b4 (inp + 1)

        state_sudone :: Int -> Offset -> Word32 -> Word32 -> IO Text
        state_sudone !out !inp !hi !lo
          | 56320 <= lo, lo <= 57343
          = writeCodePoint out inp (65536 + (shiftL (hi - 55296) 10 .|.  (lo - 56320)))

          | otherwise
          = throwDecodeError

        state_su4 :: Int -> Offset -> Word32 -> Word32 -> IO Text
        state_su4 !out !inp !hi !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_sudone out inp hi (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_sudone out inp hi (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_sudone out inp hi (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su3 :: Int -> Offset -> Word32 -> Word32 -> IO Text
        state_su3 !out !inp !hi !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su4 out (inp + 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su4 out (inp + 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su4 out (inp + 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su2 :: Int -> Offset -> Word32 -> Word32 -> IO Text
        state_su2 !out !inp !hi !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su3 out (inp + 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su3 out (inp + 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su3 out (inp + 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su1 :: Int -> Offset -> Word32 -> IO Text
        state_su1 !out !inp !hi
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su2 out (inp + 1) hi (fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su2 out (inp + 1) hi (fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su2 out (inp + 1) hi (fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su :: Int -> Offset -> Word32 -> IO Text
        state_su !out !inp !hi
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            case w8 of
              117 -> state_su1 out (inp + 1) hi
              _   -> throwDecodeError

        state_ss :: Int -> Offset -> Word32 -> IO Text
        state_ss !out !inp !hi
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            case w8 of
              92 -> state_su out (inp + 1) hi
              _  -> throwDecodeError

        state_udone :: Int -> Offset -> Word32 -> IO Text
        state_udone !out !inp !acc
          | acc < 55296 || acc > 57343 =
            writeCodePoint out inp acc

          | acc < 56320 =
            state_ss out (inp + 1) acc

          | otherwise =
            throwDecodeError

        state_u4 :: Int -> Offset -> Word32 -> IO Text
        state_u4 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u3 :: Int -> Offset -> Word32 -> IO Text
        state_u3 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u4 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u4 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u4 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u2 :: Int -> Offset -> Word32 -> IO Text
        state_u2 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u3 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u3 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u3 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u1 :: Int -> Offset -> IO Text
        state_u1 !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u2 out (inp + 1) (fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u2 out (inp + 1) (fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u2 out (inp + 1) (fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_escape :: Int -> Offset -> IO Text
        state_escape !out !inp
          | inp == end = throwDecodeError
          | otherwise  = do
            let !w8 = A.unsafeIndex src inp
            case w8 of
              34 -> do
                P.writePrimArray arr out 34
                state_start (out + 1) (inp + 1)

              92 -> do
                P.writePrimArray arr out 92
                state_start (out + 1) (inp + 1)

              47 -> do
                P.writePrimArray arr out 47
                state_start (out + 1) (inp + 1)

              98 -> do
                P.writePrimArray arr out 8
                state_start (out + 1) (inp + 1)

              102 -> do
                P.writePrimArray arr out 12
                state_start (out + 1) (inp + 1)

              110 -> do
                P.writePrimArray arr out 10
                state_start (out + 1) (inp + 1)

              114 -> do
                P.writePrimArray arr out 13
                state_start (out + 1) (inp + 1)

              116 -> do
                P.writePrimArray arr out 9
                state_start (out + 1) (inp + 1)

              117 ->
                state_u1 out (inp + 1)

              _ -> throwDecodeError

        state_start :: Int -> Offset -> IO Text
        state_start !out !inp
          | inp == end = do
            P.shrinkMutablePrimArray arr out
            frozenArr <- P.unsafeFreezePrimArray arr
            return $ case frozenArr of
              P.PrimArray ba -> T.Text (A.ByteArray ba) 0 out

          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | w8 == 92  -> state_escape out (inp + 1)
            -- because we are deserialising __valid__ text,
            -- we can simply copy everything else.
               | otherwise -> do
                 P.writePrimArray arr out w8
                 state_start (out + 1) (inp + 1)

    -- start the state machine
    state_start (0 :: Int) begin
#else

unescapeFromTextIO (Text src begin len) = do
    let end :: Offset
        end = begin + len

    arr <- P.newPrimArray len

    let state_sudone :: Int -> Offset -> Word32 -> IO Text
        state_sudone !out !inp !lo
          | 56320 <= lo, lo <= 57343 = do
            P.writePrimArray arr out (fromIntegral lo)
            state_start (out + 1) (inp + 1)

          | otherwise =
            throwDecodeError

        state_su4 :: Int -> Offset -> Word32 -> IO Text
        state_su4 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_sudone out inp (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_sudone out inp (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_sudone out inp (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su3 :: Int -> Offset -> Word32 -> IO Text
        state_su3 !out !inp  !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su4 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su4 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su4 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise -> throwDecodeError

        state_su2 :: Int -> Offset -> Word32 -> IO Text
        state_su2 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su3 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su3 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su3 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su1 :: Int -> Offset -> IO Text
        state_su1 !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su2 out (inp + 1) (fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su2 out (inp + 1) (fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su2 out (inp + 1) (fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        -- high surrogate u
        state_su :: Int -> Offset -> IO Text
        state_su !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            case w8 of
              117 -> state_su1 out (inp + 1)
              _   -> throwDecodeError

        -- high surrogate slash
        state_ss :: Int -> Offset -> IO Text
        state_ss !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            case w8 of
              92 -> state_su out (inp + 1)
              _  -> throwDecodeError

        state_udone :: Int -> Offset -> Word32 -> IO Text
        state_udone !out !inp !acc
          -- we know that codepoint in acc is in BMP
          | acc < 55296 || acc > 57343 = do
            P.writePrimArray arr out (fromIntegral acc)
            state_start (out + 1) (inp + 1)

          -- hi surrogate,
          -- we write it immediately (UTF16 as an output!)
          | acc < 56320 = do
            P.writePrimArray arr out (fromIntegral acc)
            state_ss (out + 1) (inp + 1)

          | otherwise =
            throwDecodeError

        state_u4 :: Int -> Offset -> Word32 -> IO Text
        state_u4 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u3 :: Int -> Offset -> Word32 -> IO Text
        state_u3 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u4 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u4 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u4 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u2 :: Int -> Offset -> Word32 -> IO Text
        state_u2 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u3 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u3 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u3 out (inp + 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u1 :: Int -> Offset -> IO Text
        state_u1 !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u2 out (inp + 1) (fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u2 out (inp + 1) (fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u2 out (inp + 1) (fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_escape :: Int -> Offset -> IO Text
        state_escape out inp
          | inp == end = throwDecodeError
          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            case w8 of
              34 -> do
                P.writePrimArray arr out 34
                state_start (out + 1) (inp + 1)

              92 -> do
                P.writePrimArray arr out 92
                state_start (out + 1) (inp + 1)

              47 -> do
                P.writePrimArray arr out 47
                state_start (out + 1) (inp + 1)

              98 -> do
                P.writePrimArray arr out 8
                state_start (out + 1) (inp + 1)

              102 -> do
                P.writePrimArray arr out 12
                state_start (out + 1) (inp + 1)

              110 -> do
                P.writePrimArray arr out 10
                state_start (out + 1) (inp + 1)

              114 -> do
                P.writePrimArray arr out 13
                state_start (out + 1) (inp + 1)

              116 -> do
                P.writePrimArray arr out 9
                state_start (out + 1) (inp + 1)

              117 ->
                state_u1 out (inp + 1)

              _ -> throwDecodeError

        state_start :: Int -> Offset -> IO Text
        state_start !out !inp
          | inp == end = do
            P.shrinkMutablePrimArray arr out
            frozenArr <- P.unsafeFreezePrimArray arr
            return $ case frozenArr of
              P.PrimArray ba -> T.Text (A.Array ba) 0 out

          | otherwise = do
            let !w8 = A.unsafeIndex src inp
            if | w8 == 92 -> state_escape out (inp + 1)
            -- because we are deserialising __valid__ text,
            -- we can simply copy everything else.
               | otherwise -> do
                  P.writePrimArray arr out (fromIntegral w8 :: Word16)
                  state_start (out + 1) (inp + 1)

    -- start the state machine
    state_start (0 :: Int) begin

#endif
