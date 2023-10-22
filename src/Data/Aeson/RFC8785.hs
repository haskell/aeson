{-# LANGUAGE UnboxedTuples, BangPatterns #-}
-- | JSON Canonicalization Scheme https://datatracker.ietf.org/doc/html/rfc8785
module Data.Aeson.RFC8785 (
    encodeCanonical,
) where

import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Integer (quotRemInteger)
import Math.NumberTheory.Logarithms (integerLog10)

import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Encoding.Internal
import Data.Aeson.Internal.Prelude

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Scientific as Sci
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Word8.Patterns as W8

-- $setup
-- >>> import Data.Aeson

-- | Encode to JSON according to RFC 8785 canonicalization scheme.
-- https://datatracker.ietf.org/doc/html/rfc8785
--
-- 'encodeCanonical' uses 'toJSON' to produce intermediate 'Value',
-- as 'toEncoding' may (and most likely) produces non-canonical JSON.
--
-- Note: @decode (encodeCanonical v) === Just v@ for all @v :: Value@,
-- i.e. 'encodeCanonical' doesn't lose any information.
--
-- However, the example in RFC8785 /loses/ information as the intermediate
-- number representation is 'Double', also current @toJSON :: Double -> Value@
-- sometimes produces too precise values. For example
--
-- >>> toJSON (1e23 :: Double)
-- Number 9.999999999999999e22
--
-- 'show' also behaves the same:
--
-- >>> 1e23 :: Double
-- 9.999999999999999e22
--
-- Note: RFC8785 is __not the same scheme__ as used in
-- [canonical-json](https://hackage.haskell.org/package/canonical-json) package
-- (https://wiki.laptop.org/go/Canonical_JSON).
-- That scheme produces /invalid/ JSON (e.g. control characters encoded as is, not escaped)
-- and cannot encode non-integral numbers.
--
-- @since 2.2.1.0
--
encodeCanonical :: ToJSON a => a -> LBS.ByteString
encodeCanonical = encodingToLazyByteString . toCanonical . toJSON

toCanonical :: Value -> Encoding
toCanonical Null       = null_
toCanonical (Bool b)   = bool b
toCanonical (Number n) = canonicalNumber n
toCanonical (String s) = canonicalString s
toCanonical (Array v)  = list toCanonical (V.toList v)
toCanonical (Object m) = dict (canonicalString . Key.toText) toCanonical ifr $
    sortBy (\(k1, _) (k2, _) -> propertyCmp k1 k2) (KM.toList m)

ifr :: (k -> v -> a -> a) -> a -> [(k, v)] -> a
ifr f z = foldr (\(k, v) -> f k v) z
{-# INLINE ifr #-}

-- Property name strings to be sorted are formatted as arrays of UTF-16 code units.
propertyCmp :: Key -> Key -> Ordering
propertyCmp = comparing f where
    -- this is slow implementation, but it's obviously not wrong.
    f :: Key -> BS.ByteString
    f = TE.encodeUtf16BE . Key.toText

-- strings are already serialized canonically.
canonicalString :: Text -> Encoding' a
canonicalString = text

-- RFC 8785 is outsourcing number format to ECMA-262.
-- 10th edition, 7.1.12.1 NumberToString
-- https://262.ecma-international.org/10.0/#sec-tostring-applied-to-the-number-type
--
-- Note: this specification is not lossy
-- Given 'Scientific' we can choose n,k,s uniquely: 'nks'.
--
-- RFC8785 Appendix D says "don't use bignums".
canonicalNumber :: Scientific -> Encoding
canonicalNumber m = case compare m 0 of
    EQ -> Encoding (B.word8 W8.DIGIT_0)
    LT -> Encoding (B.word8 W8.HYPHEN <> fromEncoding (canonicalNumber' (negate m)))
    GT -> canonicalNumber' m

-- input: Positive number
canonicalNumber' :: Scientific -> Encoding
canonicalNumber' m
    | k <= n, n <= 21
    = Encoding $
        BP.primMapListFixed BP.word8 ds <>
        BP.primMapListFixed BP.word8 (replicate (n - k) W8.DIGIT_0)

    | 0 < n, n <= 21
    , let (pfx, sfx) = splitAt n ds
    = Encoding $
        BP.primMapListFixed BP.word8 pfx <>
        B.word8 W8.PERIOD <>
        BP.primMapListFixed BP.word8 sfx

    | -6 < n, n <= 0
    = Encoding $
        B.word8 W8.DIGIT_0 <>
        B.word8 W8.PERIOD <>
        BP.primMapListFixed BP.word8 (replicate (negate n) W8.DIGIT_0) <>
        BP.primMapListFixed BP.word8 ds

    | k == 1, [d] <- ds
    = Encoding $
        B.word8 d <>
        B.word8 W8.LOWER_E <>
        B.word8 (if (n - 1) >= 0 then W8.PLUS else W8.HYPHEN) <>
        BP.primMapListFixed BP.word8 (integerToDecimalDigits (abs (toInteger n - 1)))

    | (d:ds') <- ds
    = Encoding $
        B.word8 d <>
        B.word8 W8.PERIOD <>
        BP.primMapListFixed BP.word8 ds' <>
        B.word8 W8.LOWER_E <>
        B.word8 (if (n - 1) >= 0 then W8.PLUS else W8.HYPHEN) <>
        BP.primMapListFixed BP.word8 (integerToDecimalDigits (abs (toInteger n - 1)))

    | otherwise
    = string "0" -- shouldn't happen, but we need a default case.

  where
    -- 5. Otherwise, let n, k, and s be integers such that
    -- k ≥ 1, 10k - 1 ≤ s < 10k, the Number value for s × 10n - k is m,
    -- and k is as small as possible.
    -- Note that k is the number of digits in the decimal representation of s,
    -- that s is not divisible by 10, and that the least significant digit of s
    -- is not necessarily uniquely determined by these criteria.
    (n, k, s) = nks m
    ds = integerToDecimalDigits s

-- 5. Otherwise, let n, k, and s be integers such that k ≥ 1, 10^(k - 1) ≤ s < 10^k,
-- the Number value for s × 10^(n - k) is m, and k is as small as possible.
-- Note that k is the number of digits in the decimal representation of s,
-- that s is not divisible by 10, and that the least significant digit of s
-- is not necessarily uniquely determined by these criteria.
nks :: Scientific -> (Int, Int, Integer)
nks m = (e + k, k, c)
  where
    m' = Sci.normalize m
    c = Sci.coefficient m'
    e = Sci.base10Exponent m'
    k = integerLog10 c + 1

integerToDecimalDigits :: Integer -> [Word8]
integerToDecimalDigits = go [] where
    go acc 0 = acc
    go acc i = case quotRemInteger i 10 of
        (# q, r #) -> go (d:acc) q where !d = fromIntegral r + W8.DIGIT_0
