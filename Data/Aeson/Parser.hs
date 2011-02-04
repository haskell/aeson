{-# LANGUAGE OverloadedStrings #-}

-- Module:      Data.Aeson.Parser
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently and correctly parse a JSON string.

module Data.Aeson.Parser
    (
      json
    , value
    ) where

import Blaze.ByteString.Builder (fromByteString, toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromChar)
import Blaze.ByteString.Builder.Word (fromWord8)
import Control.Applicative as A
import Control.Monad (when)
import Data.Aeson.Types (Value(..))
import Data.Attoparsec.Char8
import Data.Bits ((.|.), shiftL)
import Data.ByteString as B
import Data.Char (chr)
import Data.Map as Map
import Data.Monoid (mappend, mempty)
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector as Vector hiding ((++))
import Data.Word (Word8)
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Unsafe as B

-- | Parse a top-level JSON value.  This must be either an object or
-- an array.
json :: Parser Value
json = do
  c <- skipSpace *> anyChar
  case c of
    '{' -> object_
    '[' -> array_
    _   -> fail "root value is not an object or array"

object_ :: Parser Value
object_ = do
  skipSpace
  let pair = do
        a <- jstring <* skipSpace
        b <- char ':' *> skipSpace *> value
        return (a,b)
  vals <- ((pair <* skipSpace) `sepBy` (char ',' *> skipSpace)) <* char '}'
  return . Object $ Map.fromList vals

array_ :: Parser Value
array_ = do
  skipSpace
  vals <- ((value <* skipSpace) `sepBy` (char ',' *> skipSpace)) <* char ']'
  return . Array $ Vector.fromList vals

-- | Parse any JSON value.  Use 'json' in preference to this function
-- if you are parsing data from an untrusted source.
value :: Parser Value
value = most <|> (Number <$> double)
 where
  most = do
    c <- anyChar
    case c of
      '{' -> object_
      '[' -> array_
      '"' -> String <$> jstring_
      'f' -> string "alse" *> pure (Bool False)
      't' -> string "rue" *> pure (Bool True)
      'n' -> string "ull" *> pure Null
      _   -> fail "not a valid JSON value"

doubleQuote, backslash :: Word8
doubleQuote = 34
backslash = 92

jstring :: Parser Text
jstring = A.word8 doubleQuote *> jstring_

-- | Parse a string without a leading quote.
jstring_ :: Parser Text
jstring_ = do
  s <- A.scan False $ \s c -> if s then Just False
                                   else if c == doubleQuote
                                        then Nothing
                                        else Just (c == backslash)
  _ <- A.word8 doubleQuote
  if backslash `B.elem` s
    then decodeUtf8 <$> reparse unescape s
    else return (decodeUtf8 s)
{-# INLINE jstring_ #-}

reparse :: Parser a -> ByteString -> Parser a
reparse p s = case (case parse p s of {Partial k -> k ""; r -> r}) of
                Done "" r    -> return r
                Fail _ _ msg -> fail msg
                _            -> fail "unexpected failure"

unescape :: Parser ByteString
unescape = toByteString <$> go mempty where
  go acc = do
    h <- A.takeWhile (/=backslash)
    let rest = do
          start <- A.take 2
          let !slash = B.unsafeHead start
              !t = B.unsafeIndex start 1
              escape = case B.findIndex (==t) "\"\\/ntbrfu" of
                         Just i -> i
                         _      -> 255
          when (slash /= backslash || escape == 255) $
            fail "invalid JSON escape sequence"
          let continue m = go (acc `mappend` fromByteString h `mappend` m)
              {-# INLINE continue #-}
          if t /= 117 -- 'u'
            then continue (fromWord8 (B.unsafeIndex mapping escape))
            else do
                 a <- hexQuad
                 if a < 0xd800 || a > 0xdfff
                   then continue (fromChar (chr a))
                   else do
                     b <- string "\\u" *> hexQuad
                     if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
                       then let !c = ((a - 0xd800) `shiftL` 10) + (b - 0xdc00) +
                                     0x10000
                            in continue (fromChar (chr c))
                       else fail "invalid UTF-16 surrogates"
    rest <|> return (acc `mappend` fromByteString h)
  mapping = "\"\\/\n\t\b\r\f"

hexQuad :: Parser Int
hexQuad = do
  s <- A.take 4
  let hex n | w >= 48 && w <= 57  = w - 48
            | w >= 97 && w <= 122 = w - 87
            | w >= 65 && w <= 90  = w - 55
            | otherwise           = 255
        where w = fromIntegral $ B.unsafeIndex s n
      a = hex 0; b = hex 1; c = hex 2; d = hex 3
  if (a .|. b .|. c .|. d) /= 255
    then return $! d .|. (c `shiftL` 4) .|. (b `shiftL` 8) .|. (a `shiftL` 12)
    else fail "invalid hex escape"
