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

import Blaze.ByteString.Builder (Builder, fromByteString, toLazyByteString)
import Blaze.Text (double, integral)
import Data.Aeson.Types (ToJSON(..), Value(..))
import Data.Attoparsec.Number (Number(..))
import Data.Monoid (mappend)
import Data.Text.Encoding (encodeUtf8)
import Numeric (showHex)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import qualified Blaze.ByteString.Builder.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Encode a JSON value to a 'Builder'.
fromValue :: Value -> Builder
fromValue Null = {-# SCC "fromValue/Null" #-} fromByteString "null"
fromValue (Bool b) = {-# SCC "fromValue/Bool" #-}
                     if b then fromByteString "true"
                     else fromByteString "false"
fromValue (Number n) = {-# SCC "fromValue/Number" #-} fromNumber n
fromValue (String s) = {-# SCC "fromValue/String" #-} string s
fromValue (Array v)
    | V.null v = {-# SCC "fromValue/Array" #-} fromByteString "[]"
    | otherwise = {-# SCC "fromValue/Array" #-}
                  Char8.fromChar '[' `mappend`
                  fromValue (V.unsafeHead v) `mappend`
                  V.foldr f (Char8.fromChar ']') (V.unsafeTail v)
  where f a z = Char8.fromChar ',' `mappend` fromValue a `mappend` z
fromValue (Object m) = {-# SCC "fromValue/Object" #-}
    case H.toList m of
      (x:xs) -> Char8.fromChar '{' `mappend`
                one x `mappend` foldr f (Char8.fromChar '}') xs
      _      -> fromByteString "{}"
  where f a z     = Char8.fromChar ',' `mappend` one a `mappend` z
        one (k,v) = string k `mappend` Char8.fromChar ':' `mappend` fromValue v

string :: T.Text -> Builder
string s = Char8.fromChar '"' `mappend` (quote s) `mappend` Char8.fromChar '"'
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
        | c < '\x20' = Char8.fromString $
                       "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = Utf8.fromChar c
        where h = showHex (fromEnum c) ""

-- The version in blaze-builder is way slower.
fromText :: T.Text -> Builder
fromText t = fromByteString (encodeUtf8 t)

fromNumber :: Number -> Builder
fromNumber (I i) = integral i
fromNumber (D d)
    | isNaN d || isInfinite d = fromByteString "null"
    | otherwise               = double d

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
encode :: ToJSON a => a -> L.ByteString
encode = {-# SCC "encode" #-} toLazyByteString . fromValue .
         {-# SCC "toJSON" #-} toJSON
{-# INLINE encode #-}
