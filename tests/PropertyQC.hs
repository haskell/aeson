{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
module PropertyQC  (quickcheckTests) where

import Prelude.Compat

import Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, Property, testProperty, counterexample, property)
import Test.QuickCheck (shrink)
import Data.Char (isLower, isUpper, isDigit, isSpace)
import Data.Foldable (foldl')
import Data.Foldable.WithIndex (ifoldl')

import qualified Data.Text as T
import qualified Data.Aeson.Key as K
import qualified Data.Scientific as Sci

import Data.Aeson (Value (..))

quickcheckTests :: TestTree
quickcheckTests = testGroup "QuickCheck"
    [ testGroup "shrink terminates"
      [ testProperty "Int"        $ shrink_prop @Int
      , testProperty "Bool"       $ shrink_prop @Int
      , testProperty "Integer"    $ shrink_prop @Integer
      , testProperty "Char"       $ shrink_prop @Char
      , testProperty "Text"       $ shrink_prop @T.Text
      , testProperty "(Int,Int)"  $ shrink_prop @(Integer, Int)
      , testProperty "Scientific" $ shrink_prop @Sci.Scientific
      , testProperty "Value"      $ shrink_prop @Value
      ]
    ]

shrink_prop :: (Show a, ShrinkMetric a) => a -> Property
shrink_prop v = case vs' of
    []     -> property True
    v' : _ -> counterexample (show vs') $
              counterexample (show (metric v, metric v', v)) False
  where
    vs = shrink v

    -- we check only 50 first ones, otherwise it would take too long.
    vs' = filter (not . predicate) $ take 50 vs

    -- shrunk v's should be smaller.
    predicate v' = metric v' < metric v

class Arbitrary a => ShrinkMetric a where
    metric :: a -> Integer

instance (ShrinkMetric a, ShrinkMetric b) => ShrinkMetric (a, b) where
    metric (a, b) = (1 + metric a) * (1 + metric b)

instance ShrinkMetric Bool where
    metric b = if b then 1 else 0

instance ShrinkMetric Int where
    metric = metric . toInteger

instance ShrinkMetric Integer where
    metric i = if i < 0 then 1 + negate i else i

-- Char shrinking is tricky.
-- See: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/src/Test.QuickCheck.Arbitrary.html#line-664
instance ShrinkMetric Char where
    metric c = toInteger $ foldl' (+) 0
        [ if not $ isLower c then 0x2000000 else 0
        , if not $ isUpper c then 0x1000000 else 0
        , if not $ isDigit c then 0x0800000 else 0
        , if not $ c == ' '  then 0x0400000 else 0
        , if not $ isSpace c then 0x0200000 else 0
        , fromEnum c
        ]

instance ShrinkMetric T.Text where
    metric = foldl' (\acc c -> acc + 1 + metric c) 0 . T.unpack

instance ShrinkMetric K.Key where
    metric = metric . K.toText

instance ShrinkMetric Sci.Scientific where
    metric s = metric (Sci.coefficient s, Sci.base10Exponent s)

instance ShrinkMetric Value where
    metric Null        = 0
    metric (Bool b)    = 1 + metric b
    metric (String t)  = 1 + metric t
    metric (Number n)  = 1 + metric n
    metric (Array xs)  = foldl' (\acc x -> acc + 1 + metric x) 1 xs
    metric (Object xs) = ifoldl' (\k acc x -> acc + metric (k, x)) 1 xs
