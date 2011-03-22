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
import Data.Aeson.Encode.Double
import Data.Aeson.Encode.Int
import Blaze.ByteString.Builder
import GHC.Base (quotInt, remInt)
import GHC.Num (quotRemInteger)
import GHC.Types (Int(..))

#ifdef  __GLASGOW_HASKELL__
# if __GLASGOW_HASKELL__ < 611
import GHC.Integer.Internals
# else
import GHC.Integer.GMP.Internals
# endif
#endif

#ifdef INTEGER_GMP
# define PAIR(a,b) (# a,b #)
#else
# define PAIR(a,b) (a,b)
#endif

fromNumber :: Number -> Builder
fromNumber (I i) = integer i
fromNumber (D d) = double d

integer :: Integer -> Builder
integer (S# i#) = int (I# i#)
integer i
    | i < 0     = minus `mappend` go (-i)
    | otherwise = go i
  where
    go n | n < maxInt = int (fromInteger n)
         | otherwise  = putH (splitf (maxInt * maxInt) n)

    splitf p n
      | p > n       = [n]
      | otherwise   = splith p (splitf (p*p) n)

    splith p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) | q > 0     -> q : r : splitb p ns
                                  | otherwise -> r : splitb p ns
    splith _ _      = error "splith: the impossible happened."

    splitb p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) -> q : r : splitb p ns
    splitb _ _      = []

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
                PAIR(x,y)
                    | q > 0     -> int q `mappend` pblock r `mappend` putB ns
                    | otherwise -> int r `mappend` putB ns
                    where q = fromInteger x
                          r = fromInteger y
putH _ = error "putH: the impossible happened"

putB :: [Integer] -> Builder
putB (n:ns) = case n `quotRemInteger` maxInt of
                PAIR(x,y) -> pblock q `mappend` pblock r `mappend` putB ns
                    where q = fromInteger x
                          r = fromInteger y
putB _ = mempty

pblock :: Int -> Builder
pblock = go maxDigits
  where
    go !d !n
        | d == 1    = digit n
        | otherwise = go (d-1) q `mappend` digit r
        where q = n `quotInt` 10
              r = n `remInt` 10
