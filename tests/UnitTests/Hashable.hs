{-# LANGUAGE OverloadedStrings #-}
module UnitTests.Hashable (hashableLaws) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Data.Hashable (hash)

import Data.Aeson

------------------------------------------------------------------------------
-- Check that the hashes of two equal Value are the same
------------------------------------------------------------------------------

hashableLaws :: TestTree
hashableLaws = testGroup "Hashable laws" $ fmap (testCase "-")
  [ assertEqual "Hashable Object" (hash a) (hash b)
  ]
  where
  a = object ["223" .= False, "807882556" .= True]
  b = object ["807882556" .= True, "223" .= False]
