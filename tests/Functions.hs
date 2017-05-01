module Functions
    (
      appEq
    , approxEqWith
    ) where

import Prelude ()
import Prelude.Compat

appEq :: (Fractional a, Ord a) => a -> a -> Bool
appEq = approxEqWith 1e-15 1e-15

approxEqWith :: (Fractional a, Ord a) => a -> a -> a -> a -> Bool
approxEqWith maxAbsoluteError maxRelativeError a b =
    a == b || d < maxAbsoluteError ||
    d / max (abs b) (abs a) <= maxRelativeError
  where d = abs (a - b)
