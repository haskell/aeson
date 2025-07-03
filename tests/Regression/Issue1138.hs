{-# LANGUAGE OverloadedStrings #-}
module Regression.Issue1138 (issue1138) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)

import Data.Aeson

assertDecodeFailure :: Either String Value -> IO ()
assertDecodeFailure (Right v) = assertFailure $ "Unexpected success: " ++ show v
assertDecodeFailure (Left _)  = return ()

issue1138 :: TestTree
issue1138 = testGroup "Issue #1138" $ map (testCase "-")
  [ assertDecodeFailure $ eitherDecode "\"\t\""
  , assertDecodeFailure $ eitherDecode "\"\\\\\t\""

  , assertDecodeFailure $ eitherDecodeStrict "\"\t\""
  , assertDecodeFailure $ eitherDecodeStrict "\"\\\\\t\""

  , assertDecodeFailure $ eitherDecodeStrictText "\"\t\""
  , assertDecodeFailure $ eitherDecodeStrictText "\"\\\\\t\""
  ]
