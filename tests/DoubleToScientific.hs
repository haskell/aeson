{-# LANGUAGE UnboxedTuples, BangPatterns, MultiWayIf #-}
module DoubleToScientific (
    doubleToScientific,
    doubleToScientificTests,
) where

-- import Debug.Trace
-- import Data.Ratio ((%))

import Data.Bits
import Data.Foldable (foldl')
import Data.Word (Word64)
import Data.Scientific (Scientific)
import GHC.Integer (quotRemInteger)

import qualified Data.Scientific as Sci

import CastFloat
import Types (UniformWord64 (..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, counterexample, (===), (==>))

-------------------------------------------------------------------------------
-- tests
-------------------------------------------------------------------------------

-- note: RFC8785 has some special cases.
doubleToScientificTests :: TestTree
doubleToScientificTests = testGroup "doubleToScientific"
    [ testProperty "roundtrip1" $ \d ->
        Sci.toRealFloat (doubleToScientific d) === d
    , testProperty "roundtrip2" $ \(U64 w) ->
        let d = castWord64ToDouble w
        in counterexample (show d) $
            not (isInfinite d || isNaN d) ==>
            Sci.toRealFloat (doubleToScientific d) === d
    ]

-------------------------------------------------------------------------------
-- doubleToScientific
-------------------------------------------------------------------------------

{- Convert 'Double' to 'Scientific'

Based on double-conversion implementation
https://github.com/google/double-conversion

Copyright 2006-2011, the V8 project authors. All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.
    * Neither the name of Google Inc. nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
doubleToScientific :: Double -> Scientific
doubleToScientific v = case compare v 0 of
    EQ -> 0
    LT -> negate (doubleToScientific' (negate v))
    GT -> doubleToScientific' v

-------------------------------------------------------------------------------
-- doubleToScientific implementation
-------------------------------------------------------------------------------

data S = S
    { num     :: !Integer
    , den     :: !Integer
    , delta_m :: !Integer
    , delta_p :: !Integer
    , is_even :: !Bool
    }
  deriving Show

-- preconditions: v > 0
doubleToScientific' :: Double -> Scientific
doubleToScientific' v =
    let s1          = initialStartValues v
        (s2, pow10) = fixupMultiply10 s1
        ds          = generateShortestDigits s2
        k           = length ds
    in Sci.scientific (foldl' (\acc d -> 10 * acc + d) 0 ds) (pow10 - k + 1)
       -- error $ show (s1, s2, pow10, fromRational (num s2 % den s2), ds)

initialStartValues :: Double -> S
initialStartValues v = lowerBoundaryCloser' $ applyExponent e S
    { num     = shiftL s 1
    , den     = 2
    , delta_m = 1
    , delta_p = 1
    , is_even = evenInteger s
    }
  where
    (s, e, lower_boundary_is_closer) = decodeFloat' v
    lowerBoundaryCloser' = if lower_boundary_is_closer then lowerBoundaryCloser else id

-- | return significant, exponent and whether lower boundery is closer.
--
-- GHC's decodeFloat does "weird" stuff to denormal doubles,
-- that messes up our delta calculation.
decodeFloat' :: Double -> (Integer, Int, Bool)
decodeFloat' d
    | denormal   = (toInteger s,                  kDenormalExponent,              False)
    | otherwise  = (toInteger (s .|. kHiddenBit), fromIntegral e - kExponentBias, w64 .&. kSignificandMask == 0)
  where
    denormal = w64 .&. kExponentMask == 0

    s = w64 .&. kSignificandMask
    e = shiftR (w64 .&. kExponentMask) kPhysicalSignificandSize

    w64 = castDoubleToWord64 d

    kExponentMask    = 0x7FF0000000000000 :: Word64
    kSignificandMask = 0x000FFFFFFFFFFFFF :: Word64
    kHiddenBit       = 0x0010000000000000 :: Word64

    kPhysicalSignificandSize = 52 :: Int
    kExponentBias            = 0x3FF + kPhysicalSignificandSize :: Int
    kDenormalExponent        = -kExponentBias + 1 :: Int

lowerBoundaryCloser :: S -> S
lowerBoundaryCloser s = s
    { num     = shiftL (num s) 1
    , den     = shiftL (den s) 1
    , delta_p = shiftL (delta_p s) 1
    }

applyExponent :: Int -> S -> S
applyExponent e s = case compare e 0 of
    EQ -> s
    GT -> S
        { num     = shiftL (num s) e
        , den     = den s
        , delta_m = shiftL (delta_m s) e
        , delta_p = shiftL (delta_m s) e
        , is_even = is_even s
        }
    LT -> S
        { num     = num s
        , den     = shiftL (den s) (negate e)
        , delta_m = delta_m s
        , delta_p = delta_m s
        , is_even = is_even s
        }

-- This routine multiplies numerator/denominator so that its values lies in the
-- range 1-10. That is after a call to this function we have:
-- 1 <= (numerator + delta_plus) / denominator < 10.
fixupMultiply10 :: S -> (S, Int)
fixupMultiply10 = go 0 where
    go p s = case compare (den s) (num s + delta_p s) of
        GT -> go (p - 1) (times10 s)
        EQ -> (s, p) -- TODO: is_even check?
        LT -> case compare (num s + delta_p s) (10 * den s) of
            LT -> (s, p)
            _  -> go (p + 1) (div10 s)

times10 :: S -> S
times10 s = s { num = 10 * num s, delta_p = 10 * delta_p s, delta_m = 10 * delta_m s }

div10 :: S -> S
div10 s = s { den = 10 * den s }

generateShortestDigits :: S -> [Integer]
generateShortestDigits = go where
    go s = case quotRemInteger (num s) (den s) of
        (# d, r #) -> if
            | not in_delta_room_minus
            , not in_delta_room_plus
            -> d : go (times10 s { num = r })

            | in_delta_room_minus
            , in_delta_room_plus
            -- Let's see if 2*numerator < denominator.
            -- If yes, then the next digit would be < 5 and we can round down.
            -> case compare (2 * r) (den s) of
                -- Remaining digits are less than .5. -> Round down (== do nothing).
                LT -> [d]
                -- Remaining digits are more than .5 of denominator. -> Round up.
                GT -> [d+1]
                -- Halfway case.
                -- round towards even
                EQ -> if evenInteger d then [d] else [d+1]

            | in_delta_room_minus
            -- round down
            -> [d]

            | otherwise
            -- Round up.
            -> [d + 1]

          where
            in_delta_room_minus
                | is_even s = r <= delta_m s
                | otherwise = r <  delta_m s

            in_delta_room_plus
                | is_even s = r + delta_p s >= den s
                | otherwise = r + delta_p s >  den s

evenInteger :: Integer -> Bool
evenInteger i = not (testBit i 0)
