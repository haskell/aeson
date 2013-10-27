{-# LANGUAGE BangPatterns, OverloadedStrings,
             CPP, GeneralizedNewtypeDeriving, TypeFamilies #-}

-- |
-- Module:      Data.Aeson.Encode
-- Copyright:   (c) 2012 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value.
--
-- Most frequently, you'll probably want to encode straight to UTF-8
-- (the standard JSON encoding) using 'encode'.
--
-- You can convert a 'Builder' (as returned by 'fromValue') to a
-- string using e.g. 'toLazyText'.

module Data.Aeson.Encode
    ( JsonBuilder
    , toBuilder
    , jsonBuilderToLazyByteString

    , fromValue
    , encode
    ) where

import Data.Aeson.Types (ToJSON(..), JSON(..), Value(..))
import Data.Monoid (Monoid, mempty, mappend)
import Data.Scientific (Scientific, coefficient, base10Exponent, scientificBuilder)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Numeric (showHex)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))
#else
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
infixr 6 <>
#endif

-- | An instance of 'JSON' that supports efficiently building a JSON
-- string using 'toBuilder'.
newtype JsonBuilder   = JsonBuilder Builder

-- | Convert a 'JsonBuilder' to a 'Builder'.
toBuilder :: JsonBuilder -> Builder
toBuilder (JsonBuilder builder) = builder

-- | Convert a 'JsonBuilder' to a 'Builder' and convert the @Builder@
-- into a lazy 'L.ByteString'.
jsonBuilderToLazyByteString :: JsonBuilder -> L.ByteString
jsonBuilderToLazyByteString = encodeUtf8 . toLazyText . toBuilder
{-# INLINE jsonBuilderToLazyByteString #-}

data CommaMonoid = Empty | Comma !Builder

instance Monoid CommaMonoid where
  mempty = Empty
  {-# INLINE mempty #-}

  mappend Empty     r         = r
  mappend l         Empty     = l
  mappend (Comma a) (Comma b) = Comma (a <> singleton ',' <> b)
  {-# INLINE mappend #-}

runCommaMonoid :: String -> Char -> Char -> CommaMonoid -> Builder
runCommaMonoid empty _    _     Empty     = fromString empty
runCommaMonoid _     open close (Comma b) = singleton open <> b <> singleton close
{-# INLINE runCommaMonoid #-}

instance JSON JsonBuilder where
  newtype JsonObject JsonBuilder json = ObjectBuilder {unObjectBuilder :: CommaMonoid}
      deriving (Monoid)

  newtype JsonArray JsonBuilder json = ArrayBuilder {unArrayBuilder :: CommaMonoid}
      deriving (Monoid)

  jsonObject = JsonBuilder . runCommaMonoid "{}" '{' '}' . unObjectBuilder
  {-# INLINE jsonObject #-}

  jsonArray = JsonBuilder . runCommaMonoid "[]" '[' ']' . unArrayBuilder
  {-# INLINE jsonArray #-}

  jsonString = JsonBuilder . string
  {-# INLINE jsonString #-}

  jsonNumber = JsonBuilder . fromScientific
  {-# INLINE jsonNumber #-}

  jsonBool True  = JsonBuilder "true"
  jsonBool False = JsonBuilder "false"
  {-# INLINE jsonBool #-}

  jsonNull = JsonBuilder "null"
  {-# INLINE jsonNull #-}

  insert k json o = ObjectBuilder (Comma (string k <> singleton ':' <> toBuilder json)) <> o
  {-# INLINE insert #-}

  element = ArrayBuilder . Comma . toBuilder
  {-# INLINE element #-}

-- | Encode a JSON value to a 'Builder'.  You can convert this to a
-- string using e.g. 'toLazyText', or encode straight to UTF-8 (the
-- standard JSON encoding) using 'encode'.
fromValue :: Value -> Builder
fromValue Null = {-# SCC "fromValue/Null" #-} "null"
fromValue (Bool b) = {-# SCC "fromValue/Bool" #-}
                     if b then "true" else "false"
fromValue (Number s) = {-# SCC "fromValue/Number" #-} fromScientific s
fromValue (String s) = {-# SCC "fromValue/String" #-} string s
fromValue (Array v)
    | V.null v = {-# SCC "fromValue/Array" #-} "[]"
    | otherwise = {-# SCC "fromValue/Array" #-}
                  singleton '[' <>
                  fromValue (V.unsafeHead v) <>
                  V.foldr f (singleton ']') (V.unsafeTail v)
  where f a z = singleton ',' <> fromValue a <> z
fromValue (Object m) = {-# SCC "fromValue/Object" #-}
    case H.toList m of
      (x:xs) -> singleton '{' <> one x <> foldr f (singleton '}') xs
      _      -> "{}"
  where f a z     = singleton ',' <> one a <> z
        one (k,v) = string k <> singleton ':' <> fromValue v

string :: T.Text -> Builder
string s = {-# SCC "string" #-} singleton '"' <> quote s <> singleton '"'
  where
    quote q = case T.uncons t of
                Nothing      -> fromText h
                Just (!c,t') -> fromText h <> escape c <> quote t'
        where (h,t) = {-# SCC "break" #-} T.break isEscape q
    isEscape c = c == '\"' ||
                 c == '\\' ||
                 c == '<'  ||
                 c == '>'  ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"

    -- The following prevents untrusted JSON strings containing </script> or -->
    -- from causing an XSS vulnerability:
    escape '<'  = "\\u003c"
    escape '>'  = "\\u003e"

    escape c
        | c < '\x20' = fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = singleton c
        where h = showHex (fromEnum c) ""

fromScientific :: Scientific -> Builder
fromScientific s
    | e < 0     = scientificBuilder s
    | otherwise = decimal (coefficient s * 10 ^ e)
  where
    e = base10Exponent s

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
encode :: ToJSON a => a -> L.ByteString
encode = {-# SCC "encode" #-} jsonBuilderToLazyByteString .
         {-# SCC "toJSON" #-} toJSON
{-# INLINE encode #-}
