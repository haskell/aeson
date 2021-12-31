{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module PropertyQC  (quickcheckTests) where

import Prelude.Compat

import Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (shrink)

import Data.Aeson (Value)

quickcheckTests :: TestTree
quickcheckTests = testGroup "QuickCheck"
    [ testProperty "shrink terminates" shrink_prop
    ]

-- | Test that shrink eventually (in 1000000 steps at most) terminates.
shrink_prop :: Value -> Bool
shrink_prop = go 0 where
    go :: Int -> Value -> Bool
    go !n v
        | n >= 1000000 = False
        | otherwise    = case shrink v of
            []     -> True
            v' : _ -> go (n + 1) v'
        
