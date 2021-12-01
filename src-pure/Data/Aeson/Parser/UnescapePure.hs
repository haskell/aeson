{-# LANGUAGE CPP #-}
#if MIN_VERSION_text(2,0,0)
-- WARNING: This file is security sensitive as it uses unsafeWrite which does
-- not check bounds. Any changes should be made with care and we would love to
-- get informed about them, just cc us in any PR targetting this file: @eskimor @jprider63
-- We would be happy to review the changes!

-- The security check at the end (pos > length) only works if pos grows
-- monotonously, if this condition does not hold, the check is flawed.

{-# LANGUAGE CPP #-}

module Data.Aeson.Parser.UnescapePure
    (
      unescapeText
    ) where

import Control.Exception (evaluate, throw, try)
import Control.Monad (when)
import Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Array as A
import Data.Text.Encoding.Error (UnicodeException (..))
import Data.Text.Internal.Private (runText)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8, Word16, Word32)
import GHC.ST (ST)

#if MIN_VERSION_text(2,0,0)
import Data.Bits (Bits, shiftL, (.&.), (.|.))
import Data.Text.Internal.Encoding.Utf16 (chr2)
import Data.Text.Internal.Unsafe.Char (unsafeChr16, unsafeChr32, unsafeWrite)
#else
import Data.Bits (Bits, shiftL, shiftR, (.&.), (.|.))
#endif

-- Different UTF states.
data Utf =
      UtfGround
    | UtfTail1
    | UtfU32e0
    | UtfTail2
    | UtfU32ed
    | Utf843f0
    | UtfTail3
    | Utf843f4
    deriving (Eq)

data State =
      StateNone
    | StateUtf !Utf !Word32
    | StateBackslash
    | StateU0
    | StateU1 !Word16
    | StateU2 !Word16
    | StateU3 !Word16
    | StateS0 !Word16
    | StateS1 !Word16
    | StateSU0 !Word16
    | StateSU1 !Word16 !Word16
    | StateSU2 !Word16 !Word16
    | StateSU3 !Word16 !Word16
    deriving (Eq)

-- References:
-- http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
-- https://github.com/jwilm/vte/blob/master/utf8parse/src/table.rs.in

setByte1 :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte1 point word = point .|. fromIntegral (word .&. 0x3f)
{-# INLINE setByte1 #-}

setByte2 :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte2 point word = point .|. (fromIntegral (word .&. 0x3f) `shiftL` 6)
{-# INLINE setByte2 #-}

setByte2Top :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte2Top point word = point .|. (fromIntegral (word .&. 0x1f) `shiftL` 6)
{-# INLINE setByte2Top #-}

setByte3 :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte3 point word = point .|. (fromIntegral (word .&. 0x3f) `shiftL` 12)
{-# INLINE setByte3 #-}

setByte3Top :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte3Top point word = point .|. (fromIntegral (word .&. 0xf) `shiftL` 12)
{-# INLINE setByte3Top #-}

setByte4 :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte4 point word = point .|. (fromIntegral (word .&. 0x7) `shiftL` 18)
{-# INLINE setByte4 #-}

decode :: Utf -> Word32 -> Word8 -> (Utf, Word32)
decode UtfGround point word = case word of
    w | 0x00 <= w && w <= 0x7f -> (UtfGround, fromIntegral word)
    w | 0xc2 <= w && w <= 0xdf -> (UtfTail1, setByte2Top point word)
    0xe0                       -> (UtfU32e0, setByte3Top point word)
    w | 0xe1 <= w && w <= 0xec -> (UtfTail2, setByte3Top point word)
    0xed                       -> (UtfU32ed, setByte3Top point word)
    w | 0xee <= w && w <= 0xef -> (UtfTail2, setByte3Top point word)
    0xf0                       -> (Utf843f0, setByte4 point word)
    w | 0xf1 <= w && w <= 0xf3 -> (UtfTail3, setByte4 point word)
    0xf4                       -> (Utf843f4, setByte4 point word)
    _                          -> throwDecodeError

decode UtfU32e0 point word = case word of
    w | 0xa0 <= w && w <= 0xbf -> (UtfTail1, setByte2 point word)
    _                          -> throwDecodeError

decode UtfU32ed point word = case word of
    w | 0x80 <= w && w <= 0x9f -> (UtfTail1, setByte2 point word)
    _                          -> throwDecodeError

decode Utf843f0 point word = case word of
    w | 0x90 <= w && w <= 0xbf -> (UtfTail2, setByte3 point word)
    _                          -> throwDecodeError

decode Utf843f4 point word = case word of
    w | 0x80 <= w && w <= 0x8f -> (UtfTail2, setByte3 point word)
    _                          -> throwDecodeError

decode UtfTail3 point word = case word of
    w | 0x80 <= w && w <= 0xbf -> (UtfTail2, setByte3 point word)
    _                          -> throwDecodeError

decode UtfTail2 point word = case word of
    w | 0x80 <= w && w <= 0xbf -> (UtfTail1, setByte2 point word)
    _                          -> throwDecodeError

decode UtfTail1 point word = case word of
    w | 0x80 <= w && w <= 0xbf -> (UtfGround, setByte1 point word)
    _                          -> throwDecodeError

decodeHex :: Word8 -> Word16
decodeHex x
  | 48 <= x && x <=  57 = fromIntegral x - 48  -- 0-9
  | 65 <= x && x <=  70 = fromIntegral x - 55  -- A-F
  | 97 <= x && x <= 102 = fromIntegral x - 87  -- a-f
  | otherwise = throwDecodeError

unescapeText' :: ByteString -> Text
unescapeText' bs = runText $ \done -> do
    dest <- A.new len

    (pos, finalState) <- loop dest (0, StateNone) 0

    -- Check final state. Currently pos gets only increased over time, so this check should catch overflows.
    when ( finalState /= StateNone || pos > len)
      throwDecodeError

    done dest pos -- TODO: pos, pos-1??? XXX

    where
      len = B.length bs

      runUtf dest pos st point c = case decode st point c of
        (UtfGround, 92) -> -- Backslash
            return (pos, StateBackslash)
#if MIN_VERSION_text(2,0,0)
        (UtfGround, w) -> do
            d <- unsafeWrite dest pos (unsafeChr32 w)
            return (pos + d, StateNone)
#else
        (UtfGround, w) | w <= 0xffff ->
            writeAndReturn dest pos (fromIntegral w) StateNone
        (UtfGround, w) -> do
            A.unsafeWrite dest pos (0xd7c0 + fromIntegral (w `shiftR` 10))
            writeAndReturn dest (pos + 1) (0xdc00 + fromIntegral (w .&. 0x3ff)) StateNone
#endif
        (st', p) ->
            return (pos, StateUtf st' p)

      loop :: A.MArray s -> (Int, State) -> Int -> ST s (Int, State)
      loop _ ps i | i >= len = return ps
      loop dest ps i = do
        let c = B.index bs i -- JP: We can use unsafe index once we prove bounds with Liquid Haskell.
        ps' <- f dest ps c
        loop dest ps' $ i+1

      -- No pending state.
      f dest (pos, StateNone) c = runUtf dest pos UtfGround 0 c

      -- In the middle of parsing a UTF string.
      f dest (pos, StateUtf st point) c = runUtf dest pos st point c

      -- In the middle of escaping a backslash.
      f dest (pos, StateBackslash)  34 = writeAndReturn dest pos 34 StateNone -- "
      f dest (pos, StateBackslash)  92 = writeAndReturn dest pos 92 StateNone -- Backslash
      f dest (pos, StateBackslash)  47 = writeAndReturn dest pos 47 StateNone -- /
      f dest (pos, StateBackslash)  98 = writeAndReturn dest pos  8 StateNone -- b
      f dest (pos, StateBackslash) 102 = writeAndReturn dest pos 12 StateNone -- f
      f dest (pos, StateBackslash) 110 = writeAndReturn dest pos 10 StateNone -- n
      f dest (pos, StateBackslash) 114 = writeAndReturn dest pos 13 StateNone -- r
      f dest (pos, StateBackslash) 116 = writeAndReturn dest pos  9 StateNone -- t
      f    _ (pos, StateBackslash) 117 = return (pos, StateU0)                -- u
      f    _ (  _, StateBackslash) _   = throwDecodeError

      -- Processing '\u'.
      f _ (pos, StateU0) c =
        let w = decodeHex c in
        return (pos, StateU1 (w `shiftL` 12))

      f _ (pos, StateU1 w') c =
        let w = decodeHex c in
        return (pos, StateU2 (w' .|. (w `shiftL` 8)))

      f _ (pos, StateU2 w') c =
        let w = decodeHex c in
        return (pos, StateU3 (w' .|. (w `shiftL` 4)))

      f dest (pos, StateU3 w') c =
        let w = decodeHex c in
        let u = w' .|. w in

        -- Get next state based on surrogates.
        if u >= 0xd800 && u <= 0xdbff then -- High surrogate.
          return (pos, StateS0 u)
        else if u >= 0xdc00 && u <= 0xdfff then -- Low surrogate.
          throwDecodeError
        else do
#if MIN_VERSION_text(2,0,0)
          d <- unsafeWrite dest pos (unsafeChr16 u)
          return (pos + d, StateNone)
#else
          writeAndReturn dest pos u StateNone
#endif

      -- Handle surrogates.
      f _ (pos, StateS0 hi) 92 = return (pos, StateS1 hi) -- Backslash
      f _ (  _, StateS0{})  _ = throwDecodeError

      f _ (pos, StateS1 hi) 117 = return (pos, StateSU0 hi) -- u
      f _ (  _, StateS1{})   _ = throwDecodeError

      f _ (pos, StateSU0 hi) c =
        let w = decodeHex c in
        return (pos, StateSU1 hi (w `shiftL` 12))

      f _ (pos, StateSU1 hi w') c =
        let w = decodeHex c in
        return (pos, StateSU2 hi (w' .|. (w `shiftL` 8)))

      f _ (pos, StateSU2 hi w') c =
        let w = decodeHex c in
        return (pos, StateSU3 hi (w' .|. (w `shiftL` 4)))

      f dest (pos, StateSU3 hi w') c =
        let w = decodeHex c in
        let u = w' .|. w in

        -- Check if not low surrogate.
        if u < 0xdc00 || u > 0xdfff then
          throwDecodeError
        else do
#if MIN_VERSION_text(2,0,0)
          d <- unsafeWrite dest pos (chr2 hi u)
          return (pos + d, StateNone)
#else
          A.unsafeWrite dest pos hi
          writeAndReturn dest (pos + 1) u StateNone
#endif

#if MIN_VERSION_text(2,0,0)
writeAndReturn :: A.MArray s -> Int -> Word8 -> t -> ST s (Int, t)
#else
writeAndReturn :: A.MArray s -> Int -> Word16 -> t -> ST s (Int, t)
#endif
writeAndReturn dest pos char res = do
    A.unsafeWrite dest pos char
    return (pos + 1, res)
{-# INLINE writeAndReturn #-}

throwDecodeError :: a
throwDecodeError =
    let desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" in
    throw (DecodeError desc Nothing)

unescapeText :: ByteString -> Either UnicodeException Text
unescapeText = unsafeDupablePerformIO . try . evaluate . unescapeText'

#else

{-# LANGUAGE BangPatterns #-}
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
import           Data.Word                (Word16, Word32, Word8)
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (Ptr, plusPtr)
import           Foreign.Storable         (peek)

import qualified Data.ByteString.Internal as BS
import qualified Data.Primitive           as P
import qualified Data.Text.Array          as T
import qualified Data.Text.Internal       as T

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
unescapeTextIO bs = case bs of
  BS.PS fptr off len -> withForeignPtr fptr $ \bsPtr -> do
    let begin, end :: Ptr Word8
        begin = plusPtr bsPtr off
        end   = plusPtr begin len

    arr <- P.newPrimArray len

    let writeLowSurrogate :: Int -> Word16 -> Ptr Word8 -> IO Text
        writeLowSurrogate !out !w16 !inp = do
          P.writePrimArray arr out w16
          state_start (out + 1) inp

        writeCodePoint :: Int -> Ptr Word8 -> Word32 -> IO Text
        writeCodePoint !out !inp !codepoint
          | codepoint < 65536 = do
            P.writePrimArray arr out (fromIntegral codepoint)
            state_start (out + 1) (plusPtr inp 1)

          | otherwise = do
            let u = codepoint - 65536
                hi = fromIntegral (55296 + shiftR u 10) :: Word16
                lo = fromIntegral (56320 + (u .&. 1023)) :: Word16
            P.writePrimArray arr out hi
            writeLowSurrogate (out + 1) lo (plusPtr inp 1)

        state_sudone :: Int -> Ptr Word8 -> Word32 -> Word32 -> IO Text
        state_sudone !out !inp !hi !lo
          | 56320 <= lo, lo <= 57343 =
            writeCodePoint out inp (65536 + (shiftL (hi - 55296) 10 .|. (lo - 56320)))

          | otherwise =
            throwDecodeError

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
               | otherwise -> throwDecodeError

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

        -- high surrogate u
        state_su :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_su !out !inp !hi
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            case w8 of
              117 -> state_su1 out (plusPtr inp 1) hi
              _   -> throwDecodeError

        -- high surrogate slash
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
