{-# LANGUAGE CPP #-}

module Data.Aeson.Encode.Functions
    (
      brackets
    , builder
    , builder'
    , char7
    , encode
    , foldable
    , list
    , pairs
    ) where

import Data.Aeson.Encode.Builder
import Data.Aeson.Types.Class
import Data.Aeson.Types.Internal
import Data.ByteString.Builder (Builder, char7)
import Data.ByteString.Builder.Prim (primBounded)
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

-- | Encode a 'Foldable' as a JSON array.
foldable :: (Foldable t) => (a -> Encoding) -> t a -> Encoding
foldable to = brackets '[' ']' . foldMap (Value . to)
{-# INLINE foldable #-}

brackets :: Char -> Char -> Series -> Encoding
brackets begin end (Value v) = Encoding $
                               char7 begin <> fromEncoding v <> char7 end
brackets begin end Empty     = Encoding (primBounded (ascii2 (begin,end)) ())

-- | Encode a series of key/value pairs, separated by commas.
pairs :: Series -> Encoding
pairs s = brackets '{' '}' s
{-# INLINE pairs #-}
