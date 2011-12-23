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

import Data.Aeson.Types (ToJSON(..), Value(..))
import Data.Attoparsec.Number (Number(..))
import Data.Monoid (mappend)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Numeric (showHex)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Encode a JSON value to a 'Builder'.
fromValue :: Value -> Builder
fromValue Null = {-# SCC "fromValue/Null" #-} "null"
fromValue (Bool b) = {-# SCC "fromValue/Bool" #-}
                     if b then "true"
                     else "false"
fromValue (Number n) = {-# SCC "fromValue/Number" #-} fromNumber n
fromValue (String s) = {-# SCC "fromValue/String" #-} string s
fromValue (Array v)
    | V.null v = {-# SCC "fromValue/Array" #-} "[]"
    | otherwise = {-# SCC "fromValue/Array" #-}
                  singleton '[' `mappend`
                  fromValue (V.unsafeHead v) `mappend`
                  V.foldr f (singleton ']') (V.unsafeTail v)
  where f a z = singleton ',' `mappend` fromValue a `mappend` z
fromValue (Object m) = {-# SCC "fromValue/Object" #-}
    case H.toList m of
      (x:xs) -> singleton '{' `mappend`
                one x `mappend` foldr f (singleton '}') xs
      _      -> "{}"
  where f a z     = singleton ',' `mappend` one a `mappend` z
        one (k,v) = string k `mappend` singleton ':' `mappend` fromValue v

string :: T.Text -> Builder
string s = {-# SCC "string" #-}
           singleton '"' `mappend` quote s `mappend` singleton '"'
  where
    quote q = case T.uncons t of
                Just (c,t') -> fromText h `mappend` escape c `mappend` quote t'
                Nothing     -> fromText h
        where (h,t) = {-# SCC "break" #-} T.break isEscape q
    isEscape c = c == '\"' || c == '\\' || c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
        | c < '\x20' = fromString $
                       "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = singleton c
        where h = showHex (fromEnum c) ""

fromNumber :: Number -> Builder
fromNumber (I i) = decimal i
fromNumber (D d)
    | isNaN d || isInfinite d = "null"
    | otherwise               = realFloat d

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
encode :: ToJSON a => a -> L.ByteString
encode = {-# SCC "encode" #-} encodeUtf8 . toLazyText . fromValue .
         {-# SCC "toJSON" #-} toJSON
{-# INLINE encode #-}
