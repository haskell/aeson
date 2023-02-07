{-# LANGUAGE CPP #-}

-- |
-- Module:      Data.Aeson.Internal.Time
-- Copyright:   (c) 2015-2016 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable

module Data.Aeson.Internal.Time
    {-# DEPRECATED "This module will be removed in the next major aeson version." #-}
    (
      TimeOfDay64(..)
    , fromPico
    , toPico
    , diffTimeOfDay64
    , toTimeOfDay64
    ) where

import Data.Attoparsec.Time.Internal
