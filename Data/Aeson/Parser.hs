{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- |
-- Module:      Data.Aeson.Parser
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently and correctly parse a JSON string.  The string must be
-- encoded as UTF-8.

module Data.Aeson.Parser
    (
      json
    , value
    , jstring
    ) where

import Blaze.ByteString.Builder (fromByteString, toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromChar)
import Blaze.ByteString.Builder.Word (fromWord8)
import Control.Applicative as A
import Data.Aeson.Types (Value(..))
import Data.Attoparsec.Char8
import Data.Bits ((.|.), shiftL)
import Data.ByteString as B
import Data.Char (chr)
import Data.Monoid (mappend, mempty)
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector as Vector hiding ((++))
import Data.Word (Word8)
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as B
import qualified Data.HashMap.Strict as H

-- | Parse a top-level JSON value.  This must be either an object or
-- an array.
json :: Parser Value
json = do
  c <- skipSpace *> satisfy (`B8.elem` "{[")
  if c == '{'
    then object_
    else array_

object_ :: Parser Value
object_ = {-# SCC "object_" #-} do
  skipSpace
  let pair = do
        a <- jstring <* skipSpace
        b <- char ':' *> skipSpace *> value
        return (a,b)
  vals <- ((pair <* skipSpace) `sepBy` (char ',' *> skipSpace)) <* char '}'
  return . Object $ H.fromList vals

array_ :: Parser Value
array_ = {-# SCC "array_" #-} do
  skipSpace
  vals <- ((value <* skipSpace) `sepBy` (char ',' *> skipSpace)) <* char ']'
  return . Array $ Vector.fromList vals

-- | Parse any JSON value.  Use 'json' in preference to this function
-- if you are parsing data from an untrusted source.
value :: Parser Value
value = most <|> (Number <$> number)
 where
  most = do
    c <- satisfy (`B8.elem` "{[\"ftn")
    case c of
      '{' -> object_
      '[' -> array_
      '"' -> String <$> jstring_
      'f' -> string "alse" *> pure (Bool False)
      't' -> string "rue" *> pure (Bool True)
      'n' -> string "ull" *> pure Null
      _   -> error "attoparsec panic! the impossible happened!"

doubleQuote, backslash :: Word8
doubleQuote = 34
backslash = 92
{-# INLINE backslash #-}
{-# INLINE doubleQuote #-}

jstring :: Parser Text
jstring = A.word8 doubleQuote *> jstring_

-- | Parse a string without a leading quote.
jstring_ :: Parser Text
jstring_ = {-# SCC "jstring_" #-} do
  s <- A.scan False $ \s c -> if s then Just False
                                   else if c == doubleQuote
                                        then Nothing
                                        else Just (c == backslash)
  _ <- A.word8 doubleQuote
  if backslash `B.elem` s
    then case Z.parse unescape s of
           Right r  -> return (decodeUtf8 r)
           Left err -> fail err
    else return (decodeUtf8 s)
{-# INLINE jstring_ #-}

unescape :: Z.Parser ByteString
unescape = toByteString <$> go mempty where
  go acc = do
    h <- Z.takeWhile (/=backslash)
    let rest = do
          start <- Z.take 2
          let !slash = B.unsafeHead start
              !t = B.unsafeIndex start 1
              escape = case B.findIndex (==t) "\"\\/ntbrfu" of
                         Just i -> i
                         _      -> 255
          if slash /= backslash || escape == 255
            then fail "invalid JSON escape sequence"
            else do
            let cont m = go (acc `mappend` fromByteString h `mappend` m)
                {-# INLINE cont #-}
            if t /= 117 -- 'u'
              then cont (fromWord8 (B.unsafeIndex mapping escape))
              else do
                   a <- hexQuad
                   if a < 0xd800 || a > 0xdfff
                     then cont (fromChar (chr a))
                     else do
                       b <- Z.string "\\u" *> hexQuad
                       if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
                         then let !c = ((a - 0xd800) `shiftL` 10) +
                                       (b - 0xdc00) + 0x10000
                              in cont (fromChar (chr c))
                         else fail "invalid UTF-16 surrogates"
    done <- Z.atEnd
    if done
      then return (acc `mappend` fromByteString h)
      else rest
  mapping = "\"\\/\n\t\b\r\f"

hexQuad :: Z.Parser Int
hexQuad = do
  s <- Z.take 4
  let hex n | w >= 48 && w <= 57  = w - 48
            | w >= 97 && w <= 122 = w - 87
            | w >= 65 && w <= 90  = w - 55
            | otherwise           = 255
        where w = fromIntegral $ B.unsafeIndex s n
      a = hex 0; b = hex 1; c = hex 2; d = hex 3
  if (a .|. b .|. c .|. d) /= 255
    then return $! d .|. (c `shiftL` 4) .|. (b `shiftL` 8) .|. (a `shiftL` 12)
    else fail "invalid hex escape"
