{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
-- {-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
module Regression.Issue967 (issue967) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, assertEqual)

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE

import Data.Aeson
import Data.Aeson.TH

data DataA = DataA
  { val1 :: Int,
    val2 :: Int
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

$(deriveJSON defaultOptions ''DataA)

-------------------------------------------------------------------------------
-- Test
-------------------------------------------------------------------------------

issue967 :: TestTree
issue967 = testCase "issue967" $ do
  let ev = DataA 1 2
      encoding = encode ev
      parsedEv = decode encoding :: Maybe DataA

  assertEqual (LT.unpack $ LTE.decodeUtf8 encoding) (Just ev) parsedEv
