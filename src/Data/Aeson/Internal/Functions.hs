{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Functions
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable

module Data.Aeson.Internal.Functions
    ( mapTextKeyVal
    , mapKeyVal
    , mapKey
    ) where

import Prelude.Compat

import Data.Hashable (Hashable)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Text as T

-- | Transform a 'M.Map' into a 'KM.KeyMap' while transforming the keys.
mapTextKeyVal :: (k -> T.Text) -> (v1 -> v2)
              -> M.Map k v1 -> KM.KeyMap v2
mapTextKeyVal fk kv = M.foldrWithKey (\k v -> KM.insert (fk k) (kv v)) KM.empty
{-# INLINE mapTextKeyVal #-}

-- | Transform the keys and values of a 'H.HashMap'.
mapKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2)
          -> H.HashMap k1 v1 -> H.HashMap k2 v2
mapKeyVal fk kv = H.foldrWithKey (\k v -> H.insert (fk k) (kv v)) H.empty
{-# INLINE mapKeyVal #-}

-- | Transform the keys of a 'H.HashMap'.
mapKey :: (Eq k2, Hashable k2) => (k1 -> k2) -> H.HashMap k1 v -> H.HashMap k2 v
mapKey fk = mapKeyVal fk id
{-# INLINE mapKey #-}

