{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
module Data.Time.ToText (
    buildDay,
    buildLocalTime,
    buildTimeOfDay,
    buildTimeZone,
    buildUTCTime,
    buildZonedTime,
    buildYear,
    buildMonth,
    buildQuarter,
    buildQuarterOfYear,
) where

import           Data.Char                         (chr)
import           Data.Fixed                        (Fixed (..))
import           Data.Int                          (Int64)
import           Data.Text.Lazy.Builder            (Builder)

import           Data.Time                         (TimeOfDay (..))
import           Data.Time.Calendar                (Day, toGregorian)
import           Data.Time.Calendar.Compat         (Year)
import           Data.Time.Calendar.Month.Compat   (Month, toYearMonth)
import           Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear (..),
                                                    toYearQuarter)
import           Data.Time.Clock                   (UTCTime (..))

import qualified Data.Text.Lazy.Builder            as B
import qualified Data.Text.Lazy.Builder.Int        as B (decimal)
import qualified Data.Time.LocalTime               as Local

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup                    ((<>))
#endif

buildDay :: Day -> Builder
buildDay dd = buildYear yr <> char7 '-' <> digits2 m <> char7 '-' <> digits2 d
  where (yr,m,d) = toGregorian dd
{-# INLINE buildDay #-}

buildMonth :: Month -> Builder
buildMonth mm = buildYear yr <> char7 '-' <> digits2 m
  where (yr,m) = toYearMonth mm
{-# INLINE buildMonth #-}

buildQuarter :: Quarter -> Builder
buildQuarter qq = buildYear yr <> char7 '-' <> buildQuarterOfYear q
  where (yr,q) = toYearQuarter qq
{-# INLINE buildQuarter #-}

buildQuarterOfYear :: QuarterOfYear -> Builder
buildQuarterOfYear q = char7 'q' <> case q of
    Q1 -> char7 '1'
    Q2 -> char7 '2'
    Q3 -> char7 '3'
    Q4 -> char7 '4'

-- | Used in encoding day, month, quarter
buildYear :: Year -> Builder
buildYear y
    | y >= 1000 = B.decimal y
    | y >= 0    = padYear y
    | y >= -999 = char7 '-' <> padYear (negate y)
    | otherwise = B.decimal y
  where
    padYear y' =
        let (ab,c) = fromIntegral y' `quotRem` 10
            (a,b)  = ab `quotRem` 10
        in char7 '0' <> digit a <> digit b <> digit c
{-# INLINE buildYear #-}

buildTimeOfDay :: TimeOfDay -> Builder
buildTimeOfDay (TimeOfDay h m (MkFixed s)) =
    digits2 h <> char7 ':' <>
    digits2 m <> char7 ':' <>
    digits2 (fromInteger real) <> buildFrac (fromInteger frac)
  where
    (real,frac) = s `quotRem` pico

    buildFrac :: Int64 -> Builder
    buildFrac 0 = mempty
    buildFrac i = char7 '.' <> case i `quotRem` micro of
        (hi, 0)  -> buildFrac6 hi
        (hi, lo) -> digits6 hi <> buildFrac6 lo

    buildFrac6 :: Int64 -> Builder
    buildFrac6 i = case i `quotRem` milli of
        (hi, 0)  -> digits3 hi
        (hi, lo) -> digits3 hi <> digits3 lo

    digits6 i = case i `quotRem` milli of
        (hi, lo) -> digits3 hi <> digits3 lo

    digits3 i = digit64 a <> digit64 b <> digit64 c
      where
        (ab, c) = i `quotRem` 10
        (a, b) = ab `quotRem` 10

    pico       = 1000000000000 -- number of picoseconds  in 1 second
    micro      =       1000000 -- number of microseconds in 1 second
    milli      =          1000 -- number of milliseconds in 1 second
{-# INLINE buildTimeOfDay #-}

buildTimeZone :: Local.TimeZone -> Builder
buildTimeZone (Local.TimeZone off _ _)
    | off == 0  = char7 'Z'
    | otherwise = char7 s <> digits2 h <> char7 ':' <> digits2 m
  where !s         = if off < 0 then '-' else '+'
        (h,m)      = abs off `quotRem` 60
{-# INLINE buildTimeZone #-}

dayTime :: Day -> TimeOfDay -> Builder
dayTime d t = buildDay d <> char7 'T' <> buildTimeOfDay t
{-# INLINE dayTime #-}

buildUTCTime :: UTCTime -> B.Builder
buildUTCTime (UTCTime d s) = dayTime d (Local.timeToTimeOfDay s) <> char7 'Z'
{-# INLINE buildUTCTime #-}

buildLocalTime :: Local.LocalTime -> Builder
buildLocalTime (Local.LocalTime d t) = dayTime d t
{-# INLINE buildLocalTime #-}

buildZonedTime :: Local.ZonedTime -> Builder
buildZonedTime (Local.ZonedTime t z) = buildLocalTime t <> buildTimeZone z
{-# INLINE buildZonedTime #-}

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

digits2 :: Int -> Builder
digits2 a     = digit hi <> digit lo
  where (hi,lo) = a `quotRem` 10

digit :: Int -> Builder
digit x = char7 (chr (x + 48))

digit64 :: Int64 -> Builder
digit64 = digit . fromIntegral

char7 :: Char -> Builder
char7 = B.singleton
