{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}

-- Module:      Data.Aeson.Encode.Number
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a numeric JSON value as a lazy 'L.ByteString'.

module Data.Aeson.Encode.Number
    (
      fromNumber
    ) where

import Data.Monoid (mappend, mempty)
import Data.Attoparsec.Number (Number(..))
import Blaze.ByteString.Builder
import GHC.Num (quotRemInt, quotRemInteger)
import GHC.Types (Int(..))
import qualified Text.Show.ByteString as S

#ifdef  __GLASGOW_HASKELL__
# if __GLASGOW_HASKELL__ < 611
import GHC.Integer.Internals
# else
import GHC.Integer.GMP.Internals
# endif
#endif

fromNumber :: Number -> Builder
fromNumber (I i) = integer i
fromNumber (D d) = fromLazyByteString (S.show d)

integer :: Integer -> Builder
integer (S# i#) = int (I# i#)
integer i
    | i < 0     = fromWord8 45 `mappend` go (-i)
    | otherwise = go i
  where
    go n | n < maxInt = int (fromInteger n)
         | otherwise  = putH (splitf (maxInt * maxInt) n)

    splitf :: Integer -> Integer -> [Integer]
    splitf p n
      | p > n     = [n]
      | otherwise = splith p (splitf (p*p) n)

    splith :: Integer -> [Integer] -> [Integer]
    splith _ [    ] = error "splith: the impossible happened."
    splith p (n:ns) = case n `quotRemInteger` p of
#ifdef INTEGER_GMP
      (# q, r #) ->
#else
      (q, r) -> 
#endif
              if q > 0
                then q : r : splitb p ns
                else r : splitb p ns
    splitb :: Integer -> [Integer] -> [Integer]
    splitb _ [    ] = []
    splitb p (n:ns) = case n `quotRemInteger` p of
#ifdef INTEGER_GMP
      (# q, r #) ->
#else
      (q, r) ->
#endif
                q : r : splitb p ns

int :: Int -> Builder
int i
    | i < 0     = fromWord8 45 `mappend` go (-i)
    | otherwise = go i
  where
    go n | n < 10    = digit n
         | otherwise = go (n `rem` 10) `mappend` digit (n `quot` 10)

digit :: Int -> Builder
digit n = fromWord8 (fromIntegral n + 48)

data T = T !Integer !Int

fstT :: T -> Integer
fstT (T a _) = a

maxInt :: Integer
maxDigits :: Int
T maxInt maxDigits =
    until ((>mi) . (*10) . fstT) (\(T n d) -> T (n*10) (d+1)) (T 10 1)
  where mi = fromIntegral (maxBound :: Int)

putH :: [Integer] -> Builder
putH (n:ns) = case n `quotRemInteger` maxInt of
#ifdef INTEGER_GMP
  (# q', r' #) ->
#else
  (q', r') ->
#endif
    let q = fromInteger q'
        r = fromInteger r'
    in if q > 0
       then int q `mappend` pblock r `mappend` putB ns
       else int r `mappend` putB ns
putH _ = error "putH: the impossible happened"

putB :: [Integer] -> Builder
putB (n:ns) = case n `quotRemInteger` maxInt of
#ifdef INTEGER_GMP
  (# q', r' #) ->
#else
  (q', r') ->
#endif
    let q = fromInteger q'
        r = fromInteger r'
    in pblock q `mappend` pblock r `mappend` putB ns
putB _ = mempty

pblock :: Int -> Builder
pblock = go maxDigits
  where
    go !d !n
        | d == 1    = digit n
        | otherwise = go (d-1) q `mappend` digit r
        where (q, r) = n `quotRemInt` 10
