{-# LANGUAGE OverloadedStrings #-}
module Regression.Issue351 (issue351) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Lazy as L

import Data.Aeson

-- A regression test for: https://github.com/bos/aeson/issues/351
overlappingRegression :: FromJSON a => L.ByteString -> [a]
overlappingRegression bs = fromMaybe [] $ decode bs

issue351 :: TestTree
issue351 = testGroup "Issue #351" $ map (testCase "-")
  [ assertEqual "Int"  ([1, 2, 3] :: [Int])  $ overlappingRegression "[1, 2, 3]"
  , assertEqual "Char" ("abc"     :: String) $ overlappingRegression "\"abc\""
  , assertEqual "Char" (""        :: String) $ overlappingRegression "[\"a\", \"b\", \"c\"]"
  ]
