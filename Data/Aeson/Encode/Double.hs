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

import GHC.Float

import Data.ByteString.Char8 ()
import Data.Monoid
import Data.Aeson.Encode.Int
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8

double :: Double -> Builder
double = showpGFloat Nothing

-- | Show a signed RealFloat value using decimal notation when the
-- absolute value lies between 0.1 and 9,999,999, and scientific
-- notation otherwise. The optional integer can be used to specify
-- precision.
showpGFloat :: RealFloat a => Maybe Int -> a -> Builder
showpGFloat = putFormattedFloat FFGeneric

-- | Show a signed RealFloat value using decimal notation. The optional
-- integer can be used to specify precision.
showpFFloat :: RealFloat a => Maybe Int -> a -> Builder
showpFFloat = putFormattedFloat FFFixed

-- | Show a signed RealFloat value using scientific (exponential) notation.
-- The optional integer can be used to specify precision.
showpEFloat :: RealFloat a => Maybe Int -> a -> Builder
showpEFloat = putFormattedFloat FFExponent

putFormattedFloat :: RealFloat a => FFFormat -> Maybe Int -> a -> Builder
putFormattedFloat fmt decs f
  | isNaN f                   = fromChar 'N' `mappend` fromChar 'a' `mappend` fromChar 'N'
  | isInfinite f              = fromByteString (if f < 0 then "-Infinity" else "Infinity")
  | f < 0 || isNegativeZero f = fromChar '-' `mappend` go fmt (floatToDigits (toInteger base) (-f))
  | otherwise                 = go fmt (floatToDigits (toInteger base) f)
 where
 base = 10
 
 go FFGeneric p@(_,e)
   | e < 0 || e > 7 = go FFExponent p
   | otherwise      = go FFFixed    p
 go FFExponent (is, e) =
   case decs of
     Nothing -> case is of
       []     -> error "putFormattedFloat"
       [0]    -> fromByteString "0.0e0"
       [d]    -> digit d `mappend` fromByteString ".0e" `mappend` int (e-1)
       (d:ds) -> digit d `mappend` fromChar '.' `mappend` mconcat (map digit ds)
                                  `mappend` fromChar 'e' `mappend` int (e-1)
     Just dec ->
       let dec' = max dec 1 in
       case is of
         [0] -> fromChar '0' `mappend` fromChar '.' `mappend` mconcat (replicate dec' (fromChar '0'))
                  `mappend` fromChar 'e' `mappend` fromChar '0'
         _   ->
           let (ei, is') = roundTo base (dec'+1) is
               (d:ds)    = if ei > 0 then init is' else is'
           in digit d `mappend` fromChar '.' `mappend` mconcat (map digit ds)
                `mappend` fromChar 'e' `mappend` int (e - 1 + ei)
 go FFFixed (is, e) = case decs of
   Nothing
     | e <= 0    -> fromChar '0' `mappend` fromChar '.' `mappend` mconcat (replicate (-e) (fromChar '0'))
                      `mappend` mconcat (map digit is)
     | otherwise -> let g 0 rs     = fromChar '.' `mappend` mk0 rs
                        g n []     = fromChar '0' `mappend` g (n-1) []
                        g n (r:rs) = digit r `mappend` g (n-1) rs
                    in g e is
   Just dec ->
     let dec' = max dec 0 in
     if e >= 0 then
       let (ei, is') = roundTo base (dec' + e) is
           (ls,rs)   = splitAt (e+ei) is'
       in if null ls
          then mk0 ls
          else mk0 ls `mappend` (fromChar '.' `mappend` mconcat (map digit rs))
     else
       let (ei, is') = roundTo base dec' (replicate (-e) 0 ++ is)
           d:ds      = if ei > 0 then is' else 0:is'
       in if null ds
          then digit d
          else digit d `mappend` (fromChar '.' `mappend` mconcat (map digit ds))
 
 mk0 [] = fromChar '0'
 mk0 rs = mconcat (map digit rs)
