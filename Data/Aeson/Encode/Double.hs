{-# LANGUAGE BangPatterns, CPP, MagicHash, OverloadedStrings, UnboxedTuples #-}

-- Module:      Data.Aeson.Encode.Number
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a Double as a lazy 'L.ByteString'.

module Data.Aeson.Encode.Double
    (
      double
    ) where

import Blaze.ByteString.Builder (Builder, fromByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Data.Aeson.Encode.Int (digit, int, minus)
import Data.ByteString.Char8 ()
import Data.Monoid (mappend, mconcat, mempty)
import qualified Data.Vector as V

-- The code below is originally from GHC.Float, but has been optimised
-- in quite a few ways.

data T = T [Int] {-# UNPACK #-} !Int

double :: Double -> Builder
double f
    | isNaN f                   = fromByteString "NaN"
    | isInfinite f              = fromByteString $
                                  if f < 0 then "-Infinity" else "Infinity"
    | f < 0 || isNegativeZero f = minus `mappend` goGeneric (floatToDigits (-f))
    | otherwise                 = goGeneric (floatToDigits f)
  where
   goGeneric p@(T _ e)
     | e < 0 || e > 7 = goExponent p
     | otherwise      = goFixed    p
   goExponent (T is e) =
       case is of
         []     -> error "putFormattedFloat"
         [0]    -> fromByteString "0.0e0"
         [d]    -> digit d `mappend` fromByteString ".0e" `mappend` int (e-1)
         (d:ds) -> digit d `mappend` fromChar '.' `mappend` digits ds `mappend`
                   fromChar 'e' `mappend` int (e-1)
   goFixed (T is e)
       | e <= 0    = fromChar '0' `mappend` fromChar '.' `mappend`
                     mconcat (replicate (-e) (fromChar '0')) `mappend`
                     digits is
       | otherwise = let g 0 rs     = fromChar '.' `mappend` mk0 rs
                         g n []     = fromChar '0' `mappend` g (n-1) []
                         g n (r:rs) = digit r `mappend` g (n-1) rs
                     in g e is
   mk0 [] = fromChar '0'
   mk0 rs = digits rs

digits :: [Int] -> Builder
digits (d:ds) = digit d `mappend` digits ds
digits _      = mempty
{-# INLINE digits #-}

floatToDigits :: Double -> T
floatToDigits 0 = T [0] 0
floatToDigits x = T (reverse rds) k
 where
  (f0, e0)     = decodeFloat x
  (minExp0, _) = floatRange (undefined::Double)
  p = floatDigits x
  b = floatRadix x
  minExp = minExp0 - p -- the real minimum exponent
  -- Haskell requires that f be adjusted so denormalized numbers
  -- will have an impossibly low exponent.  Adjust for this.
  (# f, e #) =
   let n = minExp - e0 in
   if n > 0 then (# f0 `div` (b^n), e0+n #) else (# f0, e0 #)
  (# r, s, mUp, mDn #) =
   if e >= 0
   then let be = b^ e
        in if f == b^(p-1)
           then (# f*be*b*2, 2*b, be*b, b #)
           else (# f*be*2, 2, be, be #)
   else if e > minExp && f == b^(p-1)
        then (# f*b*2, b^(-e+1)*2, b, 1 #)
        else (# f*2, b^(-e)*2, 1, 1 #)
  k = fixup k0
   where
    k0 | b == 2 = (p - 1 + e0) * 3 `div` 10
        -- logBase 10 2 is slightly bigger than 3/10 so the following
        -- will err on the low side.  Ignoring the fraction will make
        -- it err even more.  Haskell promises that p-1 <= logBase b f
        -- < p.
       | otherwise = ceiling ((log (fromInteger (f+1) :: Double) +
                               fromIntegral e * log (fromInteger b)) / log 10)
    fixup n
      | n >= 0    = if r + mUp <= exp10 n * s then n else fixup (n+1)
      | otherwise = if exp10 (-n) * (r + mUp) <= s then n else fixup (n+1)

  gen ds !rn !sN !mUpN !mDnN =
   let (dn0, rn') = (rn * 10) `divMod` sN
       mUpN' = mUpN * 10
       mDnN' = mDnN * 10
       !dn   = fromInteger dn0
       !dn'  = dn + 1
   in case (# rn' < mDnN', rn' + mUpN' > sN #) of
        (# True,  False #) -> dn : ds
        (# False, True #)  -> dn' : ds
        (# True,  True #)  -> if rn' * 2 < sN then dn : ds else dn' : ds
        (# False, False #) -> gen (dn:ds) rn' sN mUpN' mDnN'

  rds | k >= 0    = gen [] r (s * exp10 k) mUp mDn
      | otherwise = gen [] (r * bk) s (mUp * bk) (mDn * bk)
      where bk = exp10 (-k)
                    
exp10 :: Int -> Integer
exp10 n
    | n >= 0 && n < maxExpt = V.unsafeIndex expts n
    | otherwise             = 10 ^ n
  where expts = V.generate maxExpt (10^)
        {-# NOINLINE expts #-}
        maxExpt = 17
{-# INLINE exp10 #-}
