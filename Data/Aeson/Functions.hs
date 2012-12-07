-- |
-- Module:      Data.Aeson.Functions
-- Copyright:   (c) 2011, 2012 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable

module Data.Aeson.Functions
    ( mapHashKeyVal
    , hashMapKey
    , mapKeyVal
    , mapKey
    -- * String conversions
    , decode
    , strict
    , lazy
    ) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M

-- | Transform a 'M.Map' into a 'H.HashMap' while transforming the keys.
mapHashKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2)
              -> M.Map k1 v1 -> H.HashMap k2 v2
mapHashKeyVal fk kv = M.foldrWithKey (\k v -> H.insert (fk k) (kv v)) H.empty
{-# INLINE mapHashKeyVal #-}

-- | Transform a 'M.Map' into a 'H.HashMap' while transforming the keys.
hashMapKey :: (Ord k2) => (k1 -> k2)
           -> H.HashMap k1 v -> M.Map k2 v
hashMapKey kv = H.foldrWithKey (M.insert . kv) M.empty
{-# INLINE hashMapKey #-}

-- | Transform the keys and values of a 'H.HashMap'.
mapKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2)
          -> H.HashMap k1 v1 -> H.HashMap k2 v2
mapKeyVal fk kv = H.foldrWithKey (\k v -> H.insert (fk k) (kv v)) H.empty
{-# INLINE mapKeyVal #-}

-- | Transform the keys of a 'H.HashMap'.
mapKey :: (Eq k2, Hashable k2) => (k1 -> k2) -> H.HashMap k1 v -> H.HashMap k2 v
mapKey fk = mapKeyVal fk id
{-# INLINE mapKey #-}

strict :: L.ByteString -> Text
strict = decode . B.concat . L.toChunks
{-# INLINE strict #-}

lazy :: Text -> L.ByteString
lazy = L.fromChunks . (:[]) . encodeUtf8
{-# INLINE lazy #-}

decode :: B.ByteString -> Text
decode = decodeUtf8
{-# INLINE decode #-}
