{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}

-- Module:      Data.Aeson.Encode.Int
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize an integral JSON value as a lazy 'L.ByteString'.

module Data.Aeson.Encode.Int
    (
      digit
    , int
    , minus
    ) where

import Blaze.ByteString.Builder
import Data.Monoid (mappend)

int :: Int -> Builder
int i
    | i < 0     = minus `mappend` go (-i)
    | otherwise = go i
  where
    go n | n < 10    = digit n
         | otherwise = go (n `rem` 10) `mappend` digit (n `quot` 10)

digit :: Int -> Builder
digit n = fromWord8 $! fromIntegral n + 48

minus :: Builder
minus = fromWord8 45
