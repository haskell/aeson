module Data.Aeson.Functions
    (
      hashMap
    , mapHash
    , transformMap
    -- * String conversions
    , decode
    , strict
    , lazy
    ) where

import Control.Arrow ((***), first)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M

-- | Transform one map into another.  The ordering of keys must be
-- preserved by the key transformation function.
transformMap :: (Ord k2) => (k1 -> k2) -> (v1 -> v2)
             -> M.Map k1 v1 -> M.Map k2 v2
transformMap fk fv = M.fromAscList . map (fk *** fv) . M.toAscList
{-# INLINE transformMap #-}

-- | Transform a 'H.HashMap' into a 'M.Map'.
hashMap :: (Ord k2) => (k1 -> k2) -> (v1 -> v2)
        -> H.HashMap k1 v1 -> M.Map k2 v2
hashMap fk kv = M.fromList . map (fk *** kv) . H.toList
{-# INLINE hashMap #-}

-- | Transform a 'M.Map' into a 'H.HashMap'.
mapHash :: (Eq k2, Hashable k2) => (k1 -> k2) -> M.Map k1 v -> H.HashMap k2 v
mapHash fk = H.fromList . map (first fk) . M.toList
{-# INLINE mapHash #-}

strict :: L.ByteString -> Text
strict = decode . B.concat . L.toChunks
{-# INLINE strict #-}

lazy :: Text -> L.ByteString
lazy = L.fromChunks . (:[]) . encodeUtf8
{-# INLINE lazy #-}

decode :: B.ByteString -> Text
decode = decodeUtf8
{-# INLINE decode #-}
