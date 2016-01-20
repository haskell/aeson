{-# LANGUAGE CPP #-}

-- |
-- Module:      Data.Aeson.Internal.Time
-- Copyright:   (c) 2015-2016 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable

module Data.Aeson.Internal.Time
    (
      TimeOfDay64(..)
    , fromPico
    , toPico
    , diffTimeOfDay64
    , toTimeOfDay64
    ) where

import Data.Fixed (Pico)
import Data.Int (Int64)
import Data.Time
import Unsafe.Coerce (unsafeCoerce)

#if MIN_VERSION_base(4,7,0)

import Data.Fixed (Fixed(MkFixed))

toPico :: Integer -> Pico
toPico = MkFixed

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i

#else

toPico :: Integer -> Pico
toPico = unsafeCoerce

fromPico :: Pico -> Integer
fromPico = unsafeCoerce

#endif

-- | Like TimeOfDay, but using a fixed-width integer for seconds.
data TimeOfDay64 = TOD {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int64

diffTimeOfDay64 :: DiffTime -> TimeOfDay64
diffTimeOfDay64 t = TOD (fromIntegral h) (fromIntegral m) s
  where (h,mp) = fromIntegral pico `quotRem` 3600000000000000
        (m,s)  = mp `quotRem` 60000000000000
        pico   = unsafeCoerce t :: Integer

toTimeOfDay64 :: TimeOfDay -> TimeOfDay64
toTimeOfDay64 (TimeOfDay h m s) = TOD h m (fromIntegral (fromPico s))
