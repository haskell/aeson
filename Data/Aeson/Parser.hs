{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Parser
    (
      json
    , value
    ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Monoid (Monoid(..))
import Control.Applicative as A
import Data.Aeson.Types (Value(..))
import Data.Attoparsec.Char8
import Data.Bits (shiftL)
import Data.ByteString as B
import Data.ByteString.Unsafe as B
import Data.Char (chr)
import Data.Map as Map
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector as Vector hiding ((++))
import Data.Word (Word8)
import qualified Data.Attoparsec as A

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
  decodeUtf8 <$> reparse unescape s

reparse :: Parser a -> ByteString -> Parser a
reparse p s = case (case parse p s of {Partial k -> k ""; r -> r}) of
                Done "" r    -> return r
                Fail _ _ msg -> fail msg
                _            -> fail "unexpected failure"

unescape :: Parser ByteString
unescape = toByteString <$> go mempty where
  go bld = do
    let backslash = 92
    h <- A.takeWhile (/=backslash)
    let rest = do
          w <- A.word8 backslash *> A.satisfy (`B.elem` "\"\\/ntbrfu")
          case B.findIndex (==w) "\"\\/ntbrf" of
            Just i  -> go (bld `mappend` fromByteString h `mappend` fromWord8 (B.unsafeIndex "\"\\/\n\t\b\r\f" i))
            Nothing -> do
                 a <- reparse hexadecimal =<< A.take 4
                 if a < 0xd800 || a > 0xdfff
                   then go (bld `mappend` fromByteString h `mappend` fromChar (chr a))
                   else do
                     b <- string "\\u" *> (reparse hexadecimal =<< A.take 4)
                     if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
                       then let c = ((a - 0xd800) `shiftL` 10) + (b - 0xdc00) +
                                    0x10000
                            in go (bld `mappend` fromByteString h `mappend` fromChar (chr c))
                       else fail "invalid UTF-16 surrogates"
    rest <|> return (bld `mappend` fromByteString h)
