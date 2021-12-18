{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Data.Aeson.Text
-- Copyright:   (c) 2012-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Most frequently, you'll probably want to encode straight to UTF-8
-- (the standard JSON encoding) using 'encode'.
--
-- You can use the conversions to 'Builder's when embedding JSON messages as
-- parts of a protocol.

module Data.Aeson.Text
    (
      encodeToLazyText
    , encodeToTextBuilder
    ) where

import Prelude.Compat

import Data.Aeson.Types (Value(..), ToJSON(..))
import Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (FPFormat(..), Scientific, base10Exponent)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)
import Numeric (showHex)
import qualified Data.Aeson.Key as Key
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Vector as V

-- | Encode a JSON 'Value' to a "Data.Text.Lazy"
--
-- /Note:/ uses 'toEncoding'
encodeToLazyText :: ToJSON a => a -> LT.Text
encodeToLazyText = LT.decodeUtf8 . encodingToLazyByteString . toEncoding

-- | Encode a JSON 'Value' to a "Data.Text" 'Builder', which can be
-- embedded efficiently in a text-based protocol.
--
-- If you are going to immediately encode straight to a
-- 'L.ByteString', it is more efficient to use 'encode' (lazy ByteString)
-- or @'fromEncoding' . 'toEncoding'@ (ByteString.Builder) instead.
--
-- /Note:/ Uses 'toJSON'
encodeToTextBuilder :: ToJSON a => a -> Builder
encodeToTextBuilder =
    go . toJSON
  where
    go Null       = {-# SCC "go/Null" #-} "null"
    go (Bool b)   = {-# SCC "go/Bool" #-} if b then "true" else "false"
    go (Number s) = {-# SCC "go/Number" #-} fromScientific s
    go (String s) = {-# SCC "go/String" #-} string s
    go (Array v)
        | V.null v = {-# SCC "go/Array" #-} "[]"
        | otherwise = {-# SCC "go/Array" #-}
                      TB.singleton '[' <>
                      go (V.unsafeHead v) <>
                      V.foldr f (TB.singleton ']') (V.unsafeTail v)
      where f a z = TB.singleton ',' <> go a <> z
    go (Object m) = {-# SCC "go/Object" #-}
        case KM.toList m of
          (x:xs) -> TB.singleton '{' <> one x <> foldr f (TB.singleton '}') xs
          _      -> "{}"
      where f a z     = TB.singleton ',' <> one a <> z
            one (k,v) = string (Key.toText k) <> TB.singleton ':' <> go v

string :: T.Text -> Builder
string s = {-# SCC "string" #-} TB.singleton '"' <> quote s <> TB.singleton '"'
  where
    quote q = case T.uncons t of
                Nothing      -> TB.fromText h
                Just (!c,t') -> TB.fromText h <> escape c <> quote t'
        where (h,t) = {-# SCC "break" #-} T.break isEscape q
    isEscape c = c == '\"' ||
                 c == '\\' ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"

    escape c
        | c < '\x20' = TB.fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = TB.singleton c
        where h = showHex (fromEnum c) ""

fromScientific :: Scientific -> Builder
fromScientific s = formatScientificBuilder format prec s
  where
    (format, prec)
      | base10Exponent s < 0 = (Generic, Nothing)
      | otherwise            = (Fixed,   Just 0)
