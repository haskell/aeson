{-# LANGUAGE OverloadedStrings #-}
module UnitTests.KeyMapInsertWith (keyMapInsertWithTests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Data.Aeson.KeyMap as KM

keyMapInsertWithTests :: TestTree
keyMapInsertWithTests = testCase "KeyMap.insertWith" $ do
  KM.insertWith (-)        "a" 2 (KM.fromList [("a", 1)]) @?= KM.fromList [("a",1 :: Int)]
  KM.insertWith (flip (-)) "a" 2 (KM.fromList [("a", 1)]) @?= KM.fromList [("a",-1 :: Int)]
  KM.insertWith (-)        "b" 2 (KM.fromList [("a", 1)]) @?= KM.fromList [("a",1),("b",2 :: Int)]
  KM.insertWith (-)        "b" 2 KM.empty                 @?= KM.fromList [("b",2 :: Int)]
