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

import Control.Applicative as A
import Control.Monad (when)
import Data.Aeson.Types (Value(..))
import Data.Attoparsec.Char8
import Data.Bits ((.|.), shiftL)
import Data.ByteString as B
import Data.Char (chr)
import Data.Map as Map
import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
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
    '{' -> skipSpace *> object_
    '[' -> skipSpace *> array_
    _   -> fail "root value is not an object or array"

object_ :: Parser Value
object_ = do
  let pair = liftA2 (,) (jstring <* skipSpace) (char8 ':' *> skipSpace *> value)
  vals <- ((pair <* skipSpace) `sepBy` (char8 ',' *> skipSpace)) <* char8 '}'
  return . Object $ Map.fromList vals

array_ :: Parser Value
array_ = do
  vals <- ((value <* skipSpace) `sepBy` (char8 ',' *> skipSpace)) <* char8 ']'
  return . Array $ Vector.fromList vals

-- | Parse any JSON value.  Use 'json' in preference to this function
-- if you are parsing data from an untrusted source.
value :: Parser Value
value = most <|> (Number <$> double)
 where
  most = do
    c <- anyChar
    case c of
      '{' -> skipSpace *> object_
      '[' -> skipSpace *> array_
      '"' -> String <$> jstring_
      'f' -> string "alse" *> pure (Bool False)
      't' -> string "rue" *> pure (Bool True)
      'n' -> string "ull" *> pure Null
      _   -> A.empty

doubleQuote :: Word8
doubleQuote = 34

jstring :: Parser Text
jstring = A.word8 doubleQuote *> jstring_

-- | Parse a string without a leading quote.
jstring_ :: Parser Text
jstring_ = do
  let backslash = 92
  s <- A.scan False $ \s c -> if s then Just False
                                   else if c == doubleQuote
                                        then Nothing
                                        else Just (c == backslash)
  _ <- A.word8 doubleQuote
  if backslash `B.elem` s
    then (decodeUtf8 . B.concat) <$> reparse unescape s
    else return (decodeUtf8 s)

reparse :: Parser a -> ByteString -> Parser a
reparse p s = case (case parse p s of {Partial k -> k ""; r -> r}) of
                Done "" r    -> return r
                Fail _ _ msg -> fail msg
                _            -> fail "unexpected failure"

unescape :: Parser [ByteString]
unescape = Prelude.reverse <$> go [] where
  go acc = do
    let backslash = 92
    h <- A.takeWhile (/=backslash)
    let rest = do
          start <- A.take 2
          let !slash = B.unsafeHead start
              !t = B.unsafeIndex start 1
              escape = case B.findIndex (==t) "\"\\/ntbrfu" of
                         Just i -> i
                         _      -> 255
          when (slash /= backslash || escape == 255) $
            fail "bad escape sequence"
          if t /= 117 -- 'u'
            then go (B.singleton (B.unsafeIndex mapping escape):h:acc)
            else do
                 a <- hexQuad
                 if a < 0xd800 || a > 0xdfff
                   then go (encodeUtf8 (T.singleton . chr $ a):h:acc)
                   else do
                     b <- string "\\u" *> hexQuad
                     if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
                       then let !c = ((a - 0xd800) `shiftL` 10) + (b - 0xdc00) +
                                     0x10000
                            in go (encodeUtf8 (T.singleton (chr c)):h:acc)
                       else fail "invalid UTF-16 surrogates"
    rest <|> return (h:acc)
  mapping = "\"\\/\n\t\b\r\f"

hexQuad :: Parser Int
hexQuad = do
  s <- A.take 4
  let hex n | w >= 48 && w <= 57  = w - 48
            | w >= 97 && w <= 122 = w - 87
            | w >= 65 && w <= 90  = w - 55
            | otherwise           = 255
        where w = fromIntegral $ B.unsafeIndex s n
      a = hex 0
      b = hex 1
      c = hex 2
      d = hex 3
  if (a .|. b .|. c .|. d) /= 255
    then return $! d .|. (c `shiftL` 4) .|. (b `shiftL` 8) .|. (a `shiftL` 12)
    else fail "invalid hex escape"
