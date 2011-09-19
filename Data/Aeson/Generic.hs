{-# LANGUAGE PatternGuards, RankNTypes, ScopedTypeVariables  #-}

-- |
-- Module:      Data.Aeson.Generic
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2008, 2009 Lennart Augustsson
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- JSON handling using 'Data.Generics'.
--
-- This is based on the 'Text.JSON.Generic' package originally written
-- by Lennart Augustsson.

module Data.Aeson.Generic
    (
      fromJSON
    , toJSON
    ) where

import Data.Aeson.Types.Internal (Value, Result, genericFromJSON, genericToJSON)
import Data.Data (Data)

fromJSON :: (Data a) => Value -> Result a
fromJSON = genericFromJSON

toJSON :: (Data a) => a -> Value
toJSON = genericToJSON
