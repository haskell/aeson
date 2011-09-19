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
--
-- /NOTE: This module is DEPRECATED!/
--
-- Please use 'genericFromJSON' or 'genericToJSON' from "Data.Aeson.Types" instead.

module Data.Aeson.Generic
    {-# DEPRECATED "Please use genericFromJSON or genericToJSON from Data.Aeson.Types instead" #-}
    (
      fromJSON
    , toJSON
    ) where

import Data.Aeson.Types (Value, Result, genericFromJSON, genericToJSON)
import Data.Data (Data)

{-# DEPRECATED fromJSON "Please use Data.Aeson.Types.genericFromJSON instead." #-}
fromJSON :: (Data a) => Value -> Result a
fromJSON = genericFromJSON

{-# DEPRECATED toJSON "Please use Data.Aeson.Types.genericToJSON instead." #-}
toJSON :: (Data a) => a -> Value
toJSON = genericToJSON
