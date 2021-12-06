{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MultiWayIf   #-}

module Data.Aeson.Parser.UnescapePure
  ( unescapeText
  ) where

import           Control.Exception        (throwIO, try)
import           Data.Bits                (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import           Data.Text.Encoding.Error (UnicodeException (..))
import           Data.Text.Unsafe         (unsafeDupablePerformIO)
import           Data.Word                (Word32, Word8)
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (Ptr, plusPtr)
import           Foreign.Storable         (peek)

import qualified Data.Primitive           as P
import qualified Data.Text.Array          as T
import qualified Data.Text.Internal       as T

import Data.Aeson.Internal.ByteString

#if !MIN_VERSION_text(2,0,0)
import           Data.Word                (Word16)
#endif


unescapeText :: ByteString -> Either UnicodeException Text
unescapeText = unsafeDupablePerformIO . try . unescapeTextIO

throwDecodeError :: IO a
throwDecodeError =
  let desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"
   in throwIO (DecodeError desc Nothing)

-------------------------------------------------------------------------------
-- unescapeTextIO
-------------------------------------------------------------------------------

-- This function is generated using staged-streams
-- See: https://github.com/phadej/staged/blob/master/staged-streams-unicode/src/Unicode/JSON.hs
--
-- Because @aeson@ better to not use template-haskell itself,
-- we dump the splice and prettify it by hand a bit.
--
unescapeTextIO :: ByteString -> IO Text

#if MIN_VERSION_text(2,0,0)

unescapeTextIO bs = withBS bs $ \fptr len ->
  withForeignPtr fptr $ \begin -> do
    let end :: Ptr Word8
        end = plusPtr begin len

    arr <- P.newPrimArray len

    let write3bytes :: Int -> Word8 -> Word8 -> Word8 -> Ptr Word8 -> IO Text
        write3bytes !out !b1 !b2 !b3 !inp = do
          P.writePrimArray arr out b1
          write2bytes (out + 1) b2 b3 inp

        write2bytes :: Int -> Word8 -> Word8 -> Ptr Word8 -> IO Text
        write2bytes !out !b1 !b2 !inp = do
          P.writePrimArray arr out b1
          write1byte (out + 1) b2 inp

        write1byte :: Int -> Word8 -> Ptr Word8 -> IO Text
        write1byte !out !b1 !inp = do
          P.writePrimArray arr out b1
          state_start (out + 1) inp

        writeCodePoint :: Int -> Ptr Word8 -> Word32 -> IO Text
        writeCodePoint !out !inp !acc
          | acc <= 127 = do
            P.writePrimArray arr out (fromIntegral acc :: Word8)
            state_start (out + 1) (plusPtr inp 1)

          | acc <= 2047 = do
            let b1 = fromIntegral (shiftR acc 6 .|. 192) :: Word8
            let b2 = fromIntegral ((acc .&. 63) .|. 128) :: Word8
            P.writePrimArray arr out b1
            write1byte (out + 1) b2 (plusPtr inp 1)

          | acc <= 65535 = do
            let b1 = fromIntegral (shiftR acc 12 .|. 224) :: Word8
            let b2 = fromIntegral ((shiftR acc 6 .&. 63) .|.  128) :: Word8
            let b3 = fromIntegral ((acc .&. 63) .|. 128) :: Word8
            P.writePrimArray arr out b1
            write2bytes (out + 1) b2 b3 (plusPtr inp 1)

          | otherwise = do
            let b1 = fromIntegral (shiftR acc 18 .|. 240) :: Word8
            let b2 = fromIntegral ((shiftR acc 12 .&. 63) .|. 128) :: Word8
            let b3 = fromIntegral ((shiftR acc 6 .&. 63) .|. 128) :: Word8
            let b4 = fromIntegral ((acc .&. 63) .|. 128) :: Word8
            P.writePrimArray arr out b1
            write3bytes (out + 1) b2 b3 b4 (plusPtr inp 1)

        state_sudone :: Int -> Ptr Word8 -> Word32 -> Word32 -> IO Text
        state_sudone !out !inp !hi !lo
          | 56320 <= lo, lo <= 57343
          = writeCodePoint out inp (65536 + (shiftL (hi - 55296) 10 .|.  (lo - 56320)))
          
          | otherwise
          = throwDecodeError

        state_su4 :: Int -> Ptr Word8 -> Word32 -> Word32 -> IO Text
        state_su4 !out !inp !hi !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_sudone out inp hi (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_sudone out inp hi (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_sudone out inp hi (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su3 :: Int -> Ptr Word8 -> Word32 -> Word32 -> IO Text
        state_su3 !out !inp !hi !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su4 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su4 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su4 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su2 :: Int -> Ptr Word8 -> Word32 -> Word32 -> IO Text
        state_su2 !out !inp !hi !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 -> 
                 state_su3 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su3 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su3 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su1 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_su1 !out !inp !hi
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su2 out (plusPtr inp 1) hi (fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su2 out (plusPtr inp 1) hi (fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su2 out (plusPtr inp 1) hi (fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_su !out !inp !hi
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            case w8 of
              117 -> state_su1 out (plusPtr inp 1) hi
              _   -> throwDecodeError

        state_ss :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_ss !out !inp !hi
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            case w8 of
              92 -> state_su out (plusPtr inp 1) hi
              _  -> throwDecodeError

        state_udone :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_udone !out !inp !acc
          | acc < 55296 || acc > 57343 =
            writeCodePoint out inp acc

          | acc < 56320 =
            state_ss out (plusPtr inp 1) acc

          | otherwise =
            throwDecodeError

        state_u4 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_u4 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u3 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_u3 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u2 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_u2 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u1 :: Int -> Ptr Word8 -> IO Text
        state_u1 !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u2 out (plusPtr inp 1) (fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u2 out (plusPtr inp 1) (fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u2 out (plusPtr inp 1) (fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_escape :: Int -> Ptr Word8 -> IO Text
        state_escape !out !inp
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            case w8 of
              34 -> do
                P.writePrimArray arr out 34
                state_start (out + 1) (plusPtr inp 1)

              92 -> do
                P.writePrimArray arr out 92
                state_start (out + 1) (plusPtr inp 1)

              47 -> do
                P.writePrimArray arr out 47
                state_start (out + 1) (plusPtr inp 1)

              98 -> do
                P.writePrimArray arr out 8
                state_start (out + 1) (plusPtr inp 1)

              102 -> do
                P.writePrimArray arr out 12
                state_start (out + 1) (plusPtr inp 1)

              110 -> do
                P.writePrimArray arr out 10
                state_start (out + 1) (plusPtr inp 1)

              114 -> do
                P.writePrimArray arr out 13
                state_start (out + 1) (plusPtr inp 1)

              116 -> do
                P.writePrimArray arr out 9
                state_start (out + 1) (plusPtr inp 1)

              117 ->
                state_u1 out (plusPtr inp 1)

              _ -> throwDecodeError

        state_input4c :: Int -> Ptr Word8 -> Word8 -> Word8 -> Word8 -> IO Text
        state_input4c !out !inp !b1 !b2 !b3
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128
               , let acc    = shiftL (fromIntegral (b1 .&. 7)) 18
               , let acc'   = acc .|. shiftL (fromIntegral (b2 .&. 63)) 12
               , let acc''  = acc' .|. shiftL (fromIntegral (b3 .&. 63)) 6
               , let acc''' = acc'' .|. fromIntegral (w8 .&. 63) :: Word32
               , acc''' >= 65536 && acc''' < 1114112 -> do
                 P.writePrimArray arr out b1
                 write3bytes (out + 1) b2 b3 w8 (plusPtr inp 1)

               | otherwise ->
                 throwDecodeError

        state_input4b :: Int -> Ptr Word8 -> Word8 -> Word8 -> IO Text
        state_input4b !out !inp !b1 !b2
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128 ->
                 state_input4c out (plusPtr inp 1) b1 b2 w8

               | otherwise ->
                 throwDecodeError

        state_input4 :: Int -> Ptr Word8 -> Word8 -> IO Text
        state_input4 !out !inp !b1
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128 ->
                 state_input4b out (plusPtr inp 1) b1 w8

               | otherwise ->
                 throwDecodeError

        state_input3b :: Int -> Ptr Word8 -> Word8 -> Word8 -> IO Text
        state_input3b !out !inp !b1 !b2
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128
               , let acc   = shiftL (fromIntegral (b1 .&. 15)) 12
               , let acc'  = acc .|.  shiftL (fromIntegral (b2 .&. 63)) 6
               , let acc'' = acc' .|. fromIntegral (w8 .&. 63) :: Word32
               , (acc'' >= 2048 && acc'' < 55296) || acc'' > 57343 -> do
                 P.writePrimArray arr out b1
                 write2bytes (out + 1) b2 w8 (plusPtr inp 1)

               | otherwise ->
                 throwDecodeError

        state_input3 :: Int -> Ptr Word8 -> Word8 -> IO Text
        state_input3 !out !inp !b1
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128 ->
                 state_input3b out (plusPtr inp 1) b1 w8

               | otherwise ->
                 throwDecodeError

        state_input2 :: Int -> Ptr Word8 -> Word8 -> IO Text
        state_input2 !out !inp !b1
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128,
                 let acc = shiftL (fromIntegral (b1 .&. 63)) 6 :: Word32
                     acc' = acc .|. fromIntegral (w8 .&. 63) :: Word32
               , acc' >= 128 -> do
                 P.writePrimArray arr out b1
                 write1byte (out + 1) w8 (plusPtr inp 1)

               | otherwise ->
                 throwDecodeError

        state_start :: Int -> Ptr Word8 -> IO Text
        state_start !out !inp
          | inp == end = do
            P.shrinkMutablePrimArray arr out
            frozenArr <- P.unsafeFreezePrimArray arr
            return $ case frozenArr of
              P.PrimArray ba -> T.Text (T.ByteArray ba) 0 out

          | otherwise = do
            w8 <- peek inp
            if | w8 == 92 -> state_escape out (plusPtr inp 1)
               | w8 < 128 -> do
                 P.writePrimArray arr out w8
                 state_start (out + 1) (plusPtr inp 1)

               | w8 < 192 -> throwDecodeError
               | w8 < 224 -> state_input2 out (plusPtr inp 1) w8
               | w8 < 240 -> state_input3 out (plusPtr inp 1) w8
               | w8 < 248 -> state_input4 out (plusPtr inp 1) w8

               | otherwise -> throwDecodeError

    -- start the state machine
    state_start (0 :: Int) begin
#else

unescapeTextIO bs = withBS bs $ \fptr len ->
  withForeignPtr fptr $ \begin -> do
    let end :: Ptr Word8
        end = plusPtr begin len

    arr <- P.newPrimArray len

    let writeLowSurrogate :: Int -> Word16 -> Ptr Word8 -> IO Text
        writeLowSurrogate !out !w16 !inp = do
          P.writePrimArray arr out w16
          state_start (out + 1) inp

        state_sudone :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_sudone !out !inp !lo
          | 56320 <= lo, lo <= 57343 = do
            P.writePrimArray arr out (fromIntegral lo)
            state_start (out + 1) (plusPtr inp 1)

          | otherwise =
            throwDecodeError

        state_su4 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_su4 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_sudone out inp (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_sudone out inp (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_sudone out inp (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su3 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_su3 !out !inp  !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise -> throwDecodeError

        state_su2 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_su2 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su1 :: Int -> Ptr Word8 -> IO Text
        state_su1 !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su2 out (plusPtr inp 1) (fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su2 out (plusPtr inp 1) (fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su2 out (plusPtr inp 1) (fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        -- high surrogate u
        state_su :: Int -> Ptr Word8 -> IO Text
        state_su !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            case w8 of
              117 -> state_su1 out (plusPtr inp 1)
              _   -> throwDecodeError

        -- high surrogate slash
        state_ss :: Int -> Ptr Word8 -> IO Text
        state_ss !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            case w8 of
              92 -> state_su out (plusPtr inp 1)
              _  -> throwDecodeError

        state_udone :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_udone !out !inp !acc
          -- we know that codepoint in acc is in BMP
          | acc < 55296 || acc > 57343 = do
            P.writePrimArray arr out (fromIntegral acc)
            state_start (out + 1) (plusPtr inp 1)

          -- hi surrogate,
          -- we write it immediately (UTF16 as an output!)
          | acc < 56320 = do
            P.writePrimArray arr out (fromIntegral acc)
            state_ss (out + 1) (plusPtr inp 1)

          | otherwise =
            throwDecodeError

        state_u4 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_u4 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u3 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_u3 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u2 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_u2 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u1 :: Int -> Ptr Word8 -> IO Text
        state_u1 !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u2 out (plusPtr inp 1) (fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u2 out (plusPtr inp 1) (fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u2 out (plusPtr inp 1) (fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_escape :: Int -> Ptr Word8 -> IO Text
        state_escape out inp
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            case w8 of
              34 -> do
                P.writePrimArray arr out 34
                state_start (out + 1) (plusPtr inp 1)

              92 -> do
                P.writePrimArray arr out 92
                state_start (out + 1) (plusPtr inp 1)

              47 -> do
                P.writePrimArray arr out 47
                state_start (out + 1) (plusPtr inp 1)

              98 -> do
                P.writePrimArray arr out 8
                state_start (out + 1) (plusPtr inp 1)

              102 -> do
                P.writePrimArray arr out 12
                state_start (out + 1) (plusPtr inp 1)

              110 -> do
                P.writePrimArray arr out 10
                state_start (out + 1) (plusPtr inp 1)

              114 -> do
                P.writePrimArray arr out 13
                state_start (out + 1) (plusPtr inp 1)

              116 -> do
                P.writePrimArray arr out 9
                state_start (out + 1) (plusPtr inp 1)

              117 ->
                state_u1 out (plusPtr inp 1)

              _ -> throwDecodeError

        state_input4c :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_input4c !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | (w8 .&. 0xc0) == 0x80
               , let acc' = acc .|.  fromIntegral (w8 .&. 63)
               , acc' >= 65536 && acc' < 1114112 ->
                 if {- -- | acc' < 65536 -> do
                       --   P.writePrimArray arr out (fromIntegral acc' :: Word16)
                       --   state_start (out + 1) (plusPtr inp 1) -}
                    | otherwise -> do
                      let u = acc' - 65536
                          hi = fromIntegral (55296 + shiftR u 10) :: Word16
                          lo = fromIntegral (56320 + (u .&. 1023)) :: Word16
                      P.writePrimArray arr out hi
                      writeLowSurrogate (out + 1) lo (plusPtr inp 1)

               | otherwise -> throwDecodeError

        state_input4b :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_input4b !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | (w8 .&. 0xc0) == 0x80 ->
                 state_input4c out (plusPtr inp 1) (acc .|. shiftL (fromIntegral (w8 .&. 63)) 6)

               | otherwise -> throwDecodeError

        state_input4 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_input4 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | (w8 .&. 0xc0) == 0x80 ->
                 state_input4b out (plusPtr inp 1) (acc .|. shiftL (fromIntegral (w8 .&. 63)) 12)

               | otherwise -> throwDecodeError

        state_input3b :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_input3b !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | (w8 .&. 0xc0) == 0x80
               , let acc' = acc .|.  fromIntegral (w8 .&. 63)
               , (acc' >= 2048 && acc' < 55296) || acc' > 57343 ->
                 if | acc' < 65536 -> do
                      P.writePrimArray arr out (fromIntegral acc')
                      state_start (out + 1) (plusPtr inp 1)

                    | otherwise -> do
                      let u = acc' - 65536
                          hi = fromIntegral (55296 + shiftR u 10) :: Word16
                          lo = fromIntegral (56320 + (u .&. 1023)) :: Word16
                      P.writePrimArray arr out hi
                      writeLowSurrogate (out + 1) lo (plusPtr inp 1)

               | otherwise -> throwDecodeError

        state_input3 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_input3 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | (w8 .&. 0xc0) == 0x80 ->
                 state_input3b out (plusPtr inp 1) (acc .|. shiftL (fromIntegral (w8 .&. 63)) 6)
               | otherwise -> throwDecodeError

        state_input2 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_input2 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | (w8 .&. 0xc0) == 0x80
               , let acc' = acc .|. fromIntegral (w8 .&. 63)
               , acc' >= 0x80 -> do
                 P.writePrimArray arr out (fromIntegral acc' :: Word16)
                 state_start (out + 1) (plusPtr inp 1)

               | otherwise -> throwDecodeError

        state_start :: Int -> Ptr Word8 -> IO Text
        state_start !out !inp
          | inp == end = do
            P.shrinkMutablePrimArray arr out
            frozenArr <- P.unsafeFreezePrimArray arr
            return $ case frozenArr of
              P.PrimArray ba -> T.Text (T.Array ba) 0 out

          | otherwise = do
            w8 <- peek inp
            if | w8 == 92 -> state_escape out (plusPtr inp 1)
               | w8 < 0x80 -> do
                  P.writePrimArray arr out (fromIntegral w8 :: Word16)
                  state_start (out + 1) (plusPtr inp 1)

               | w8 < 0xc0 -> throwDecodeError
               | w8 < 224 -> state_input2 out (plusPtr inp 1) (shiftL (fromIntegral (w8 .&. 63)) 6)
               | w8 < 240 -> state_input3 out (plusPtr inp 1) (shiftL (fromIntegral (w8 .&. 15)) 12)
               | w8 < 248 -> state_input4 out (plusPtr inp 1) (shiftL (fromIntegral (w8 .&.  7)) 18)
               | otherwise -> throwDecodeError

    -- start the state machine
    state_start (0 :: Int) begin

#endif
