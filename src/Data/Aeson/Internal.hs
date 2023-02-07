{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Internal
-- Copyright:   (c) 2015-2016 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Internal types and functions.
--
-- __Note__: all declarations in this module are unstable, and prone
-- to being changed at any time.

module Data.Aeson.Internal
    {-# DEPRECATED "All contents of this module are exported from Data.Aeson.Types module. This module will be removed in the next major aeson version." #-}
    (
      IResult(..)
    , JSONPathElement(..)
    , JSONPath
    , (<?>)
    , formatError
    , ifromJSON
    , iparse
    ) where

import Data.Aeson.Types.Internal
import Data.Aeson.Types.FromJSON (ifromJSON)
