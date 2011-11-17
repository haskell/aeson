{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Data.Aeson.Encode
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value as a lazy 'L.ByteString',
-- encoded as UTF-8.

module Data.Aeson.Encode
    (
      fromValue
    , encode
    ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Attoparsec.Number (Number(..))
import Data.Aeson.Types (ToJSON(..), Value(..))
import Data.Monoid (mappend)
import Numeric (showHex)
import Blaze.Text (double, integral)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

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
    case H.toList m of
      (x:xs) -> fromChar '{' `mappend`
                one x `mappend`
                foldr f (fromChar '}') xs
      _ -> fromByteString "{}"
  where f a z     = fromChar ',' `mappend` one a `mappend` z
        one (k,v) = string k `mappend` fromChar ':' `mappend` fromValue v

string :: T.Text -> Builder
string s = fromChar '"' `mappend` quote s `mappend` fromChar '"'
  where
    quote q = case T.uncons t of
                Just (c,t') -> fromText h `mappend` escape c `mappend` quote t'
                Nothing     -> fromText h
        where (h,t) = T.break isEscape q
    isEscape c = c == '\"' || c == '\\' || c < '\x20'
    escape '\"' = fromByteString "\\\""
    escape '\\' = fromByteString "\\\\"
    escape '\n' = fromByteString "\\n"
    escape '\r' = fromByteString "\\r"
    escape '\t' = fromByteString "\\t"
    escape c
        | c < '\x20' = fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = fromChar c
        where h = showHex (fromEnum c) ""

fromNumber :: Number -> Builder
fromNumber (I i) = integral i
fromNumber (D d)
    | isNaN d || isInfinite d = fromByteString "null"
    | otherwise               = double d

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
encode :: ToJSON a => a -> L.ByteString
encode = toLazyByteString . fromValue . toJSON
{-# INLINE encode #-}
