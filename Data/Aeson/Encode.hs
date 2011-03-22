{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- Module:      Data.Aeson.Encode
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value as a lazy 'L.ByteString'.

module Data.Aeson.Encode
    (
      fromValue
    , encode
    ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Aeson.Encode.Number (fromNumber)
import Data.Aeson.Types (ToJSON(..), Value(..))
import Data.Monoid (mappend)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Blaze.ByteString.Builder.Internal
import qualified Data.Text.Fusion as F
import Foreign

-- | Encode a JSON value to a 'Builder'.
fromValue :: Value -> Builder
fromValue Null = fromByteString "null"
fromValue (Bool b) = fromByteString $ if b then "true" else "false"
fromValue (Number n) = fromNumber n
fromValue (String s) = string s
fromValue (Array v)
    | V.null v = fromByteString "[]"
    | otherwise = fromChar '[' `mappend`
                  fromValue (V.unsafeHead v) `mappend`
                  V.foldr f (fromChar ']') (V.unsafeTail v)
  where f a z = fromChar ',' `mappend` fromValue a `mappend` z
fromValue (Object m) =
    case M.toList m of
      (x:xs) -> fromChar '{' `mappend`
                one x `mappend`
                foldr f (fromChar '}') xs
      _ -> fromByteString "{}"
  where f a z     = fromChar ',' `mappend` one a `mappend` z
        one (k,v) = string k `mappend` fromChar ':' `mappend` fromValue v


-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
encode :: ToJSON a => a -> L.ByteString
encode = toLazyByteString . fromValue . toJSON
{-# INLINE encode #-}


-- TODO: Gain more speed by serialization of text values without prior
-- unpacking to lists.
string :: T.Text -> Builder
string s = 
    fromChar '"' `mappend` fromWriteText writeJSONChar s `mappend` fromChar '"'

{-# INLINE writeJSONChar #-}
writeJSONChar :: Char -> Write
writeJSONChar c = 
    -- FIXME: This interface to Write's is error-prone and hacky.
    -- The system-io-write library already provides a better one.
    boundedWrite 6 $ getPoke $ case c of
      '\"' -> writeByteString "\\\""
      '\\' -> writeByteString "\\\\"
      '\n' -> writeByteString "\\n"
      '\r' -> writeByteString "\\r"
      '\t' -> writeByteString "\\t"
      _ | c < '\x20' -> writeChar '\\'  `mappend` 
                        writeChar 'u'   `mappend` 
                        -- FIXME: Why do we have to use Word16 where Word8 would suffice?
                        writeWord16Hex (fromIntegral $ fromEnum c)
        | otherwise  -> writeChar c

-- TODO: Extend blaze-builder or better the new bytestring lib with support for
-- hex encoding.

{-# INLINE writeWord16Hex #-}
writeWord16Hex :: Word16 -> Write
writeWord16Hex = exactWrite 4 . pokeWord16Hex

{-# INLINE pokeWord16Hex #-}
pokeWord16Hex :: Word16 -> Ptr Word8 -> IO ()
pokeWord16Hex x op = do
    pokeNibble 0 12
    pokeNibble 1  8
    pokeNibble 2  4
    pokeNibble 3  0
  where
    pokeNibble off s
        | n <  10   = pokeWord8 off (fromIntegral $ 48 + n)
        | otherwise = pokeWord8 off (fromIntegral $ 55 + n)
        where
          -- TODO: Could we gain speed using an unchecked shift?
          n = shiftR x s .&. 0xF

    pokeWord8 :: Int -> Word8 -> IO ()
    pokeWord8 off = poke (op `plusPtr` off)

-- | Write a 'F.Stream' one element at a time to a builder using the given
-- write operation.
{-# INLINE fromWriteStream #-}
fromWriteStream :: (a -> Write) -> F.Stream a -> Builder
fromWriteStream write = 
    makeBuilder
  where
    makeBuilder (F.Stream next s0 _) = fromBuildStepCont $ step s0
      where
        step s1 k !(BufRange op0 ope0) = go s1 op0
          where
            go s !op = case next s of
              F.Done       -> do let !br' = BufRange op ope0
                                 k br'
              F.Skip s'    -> go s' op
              F.Yield x s' -> 
                let maxSize = getBound (write x)
                    result
                      | op `plusPtr` maxSize <= ope0 = do
                          !op' <- runWrite (write x) op
                          go s' op'
                      | otherwise  = do return $ bufferFull maxSize op (step s k)
                in result

{-# INLINE fromWriteText #-}
fromWriteText :: (Char -> Write) -> T.Text -> Builder
fromWriteText w = fromWriteStream w . F.stream

{-
fromWriteList :: (a -> Write) -> [a] -> Builder
fromWriteList write = 
    makeBuilder
  where
    makeBuilder xs0 = fromBuildStepCont $ step xs0
      where
        step xs1 k !(BufRange op0 ope0) = go xs1 op0
          where
            go [] !op = do
               let !br' = BufRange op ope0
               k br'

            go xs@(x':xs') !op
              | op `plusPtr` maxSize <= ope0 = do
                  !op' <- runPoke wio op
                  go xs' op'
              | otherwise = return $ bufferFull maxSize op (step xs k)
              where
                Write maxSize wio = write x'
{-# INLINE fromWriteList #-}
-}


