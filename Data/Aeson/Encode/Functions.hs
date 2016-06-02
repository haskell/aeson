{-# LANGUAGE CPP #-}

module Data.Aeson.Encode.Functions
    (
      brackets
    , builder
    , builder'
    , char7
    , encode
    , list
    , encodeMap
    , encodeWithKey
    ) where

import Data.Aeson.Encode.Builder
import Data.Aeson.Types.Class
import Data.Aeson.Types.Internal
import Data.ByteString.Builder (Builder, char7)
import Data.ByteString.Builder.Prim (primBounded)
import Data.Foldable (toList)
import Data.Monoid ((<>))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mempty)
#endif

list :: (a -> Encoding) -> [a] -> Encoding
list = listEncoding

builder :: ToJSON a => a -> Builder
builder = fromEncoding . toEncoding
{-# INLINE builder #-}

builder' :: (a -> Encoding) -> a -> Builder
builder' f = fromEncoding . f
{-# INLINE builder' #-}

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
--
-- This is implemented in terms of the 'ToJSON' class's 'toEncoding' method.
encode :: ToJSON a => a -> L.ByteString
encode = B.toLazyByteString . builder
{-# INLINE encode #-}

brackets :: Char -> Char -> Series -> Encoding
brackets begin end (Value v) = Encoding $
                               char7 begin <> fromEncoding v <> char7 end
brackets begin end Empty     = Encoding (primBounded (ascii2 (begin,end)) ())



encodeMap :: (k -> Encoding)
          -> (v -> Encoding)
          -> (m -> Maybe ((k,v), m))
          -> ((k -> v -> B.Builder -> B.Builder) -> B.Builder -> m -> B.Builder)
          -> m -> Encoding
encodeMap encodeKey encodeVal minViewWithKey foldrWithKey xs =
    case minViewWithKey xs of
      Nothing         -> Encoding $ primBounded (ascii2 ('{', '}')) ()
      Just ((k,v),ys) -> Encoding $
                         B.char7 '{' <> encodeKV encodeKey encodeVal k v <>
                         foldrWithKey go (B.char7 '}') ys
  where go k v b = B.char7 ',' <> encodeKV encodeKey encodeVal k v <> b
{-# INLINE encodeMap #-}

encodeWithKey :: (k -> Encoding)
              -> (v -> Encoding)
              -> ((k -> v -> Series -> Series) -> Series -> m -> Series)
              -> m -> Encoding
encodeWithKey encodeKey encodeVal foldrWithKey = brackets '{' '}' . foldrWithKey go mempty
  where go k v c = Value (Encoding $ encodeKV encodeKey encodeVal k v) <> c
{-# INLINE encodeWithKey #-}

encodeKV :: (k -> Encoding) -> (v -> Encoding) -> k -> v -> B.Builder
encodeKV encodeKey encodeVal k v = fromEncoding (encodeKey k) <> B.char7 ':' <> builder' encodeVal v
{-# INLINE encodeKV #-}
