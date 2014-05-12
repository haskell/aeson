{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- |
-- Module:      Data.Aeson.EncodeUtf8
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2013 Simon Meier <iridcode@gmail.com>
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value using the UTF-8 encoding.

module Data.Aeson.Encode.ByteString
    ( encode
    , encodeToByteStringBuilder
    ) where

import Prelude hiding (null)
import Data.Aeson.Types (ToJSON(..), Value(..))
import Data.Char (ord)
import Data.Scientific (Scientific, coefficient, base10Exponent)
import Data.Word (Word8)
import Data.Monoid (mappend)
import           Data.ByteString.Builder      as B
import           Data.ByteString.Builder.Prim as BP
import           Data.ByteString.Builder.Scientific (scientificBuilder)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
{-# INLINE (<>) #-}
infixr 6 <>

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
encode :: ToJSON a => a -> L.ByteString
encode = B.toLazyByteString . encodeToByteStringBuilder . toJSON

-- | Encode a JSON value to a ByteString 'B.Builder'. Use this function if you
-- must prepend or append further bytes to the encoded JSON value.
encodeToByteStringBuilder :: Value -> Builder
encodeToByteStringBuilder Null       = null
encodeToByteStringBuilder (Bool b)   = bool b
encodeToByteStringBuilder (Number n) = number n
encodeToByteStringBuilder (String s) = string s
encodeToByteStringBuilder (Array v)  = array v
encodeToByteStringBuilder (Object m) = object m

null :: Builder
null = BP.primBounded (ascii4 ('n',('u',('l','l')))) ()

bool :: Bool -> Builder
bool = BP.primBounded (BP.condB id (ascii4 ('t',('r',('u','e'))))
                                   (ascii5 ('f',('a',('l',('s','e'))))))

array :: V.Vector Value -> Builder
array v
  | V.null v  = B.char8 '[' <> B.char8 ']'
  | otherwise = B.char8 '[' <>
                encodeToByteStringBuilder (V.unsafeHead v) <>
                V.foldr withComma (B.char8 ']') (V.unsafeTail v)
  where
    withComma a z = B.char8 ',' <> encodeToByteStringBuilder a <> z

object :: HMS.HashMap T.Text Value -> Builder
object m = case HMS.toList m of
    (x:xs) -> B.char8 '{' <> one x <> foldr withComma (B.char8 '}') xs
    _      -> B.char8 '{' <> B.char8 '}'
  where
    withComma a z = B.char8 ',' <> one a <> z
    one (k,v)     = string k <> B.char8 ':' <> encodeToByteStringBuilder v

string :: T.Text -> B.Builder
string t =
    B.char8 '"' <> TE.encodeUtf8BuilderEscaped escapeAscii t <> B.char8 '"'
  where
    escapeAscii :: BP.BoundedPrim Word8
    escapeAscii =
        BP.condB (== c2w '\\'  ) (ascii2 ('\\','\\')) $
        BP.condB (== c2w '\"'  ) (ascii2 ('\\','"' )) $
        BP.condB (>= c2w '\x20') (BP.liftFixedToBounded BP.word8) $
        BP.condB (== c2w '\n'  ) (ascii2 ('\\','n' )) $
        BP.condB (== c2w '\r'  ) (ascii2 ('\\','r' )) $
        BP.condB (== c2w '\t'  ) (ascii2 ('\\','t' )) $
        (BP.liftFixedToBounded hexEscape) -- fallback for chars < 0x20

    c2w = fromIntegral . ord

    hexEscape :: BP.FixedPrim Word8
    hexEscape = (\c -> ('\\', ('u', fromIntegral c))) BP.>$<
        BP.char8 BP.>*< BP.char8 BP.>*< BP.word16HexFixed

number :: Scientific -> Builder
number s
    | e < 0     = scientificBuilder s
    | otherwise = B.integerDec (coefficient s * 10 ^ e)
  where
    e = base10Exponent s


{-# INLINE ascii2 #-}
ascii2 :: (Char, Char) -> BP.BoundedPrim a
ascii2 cs = BP.liftFixedToBounded $ (const cs) BP.>$< BP.char7 BP.>*< BP.char7

{-# INLINE ascii4 #-}
ascii4 :: (Char, (Char, (Char, Char))) -> BP.BoundedPrim a
ascii4 cs = BP.liftFixedToBounded $ (const cs) >$<
    BP.char7 BP.>*< BP.char7 BP.>*< BP.char7 BP.>*< BP.char7

{-# INLINE ascii5 #-}
ascii5 :: (Char, (Char, (Char, (Char, Char)))) -> BP.BoundedPrim a
ascii5 cs = BP.liftFixedToBounded $ (const cs) >$<
    BP.char7 BP.>*< BP.char7 BP.>*< BP.char7 BP.>*< BP.char7 BP.>*< BP.char7
