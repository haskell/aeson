{-# LANGUAGE OverloadedStrings, DeriveGeneric  #-}
module Regression.Issue571 (issue571) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import GHC.Generics (Generic)

import Data.Aeson

data F = F
    { a :: Maybe Int
    , b :: Maybe Int
    }
  deriving (Eq, Show, Generic)

instance FromJSON F where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = False } -- default

issue571 :: TestTree
issue571 = testCase "issue571" $ do
    -- the Maybe fields can be omitted.
    let actual = decode "{}" :: Maybe F
    actual @?= Just F { a = Nothing, b = Nothing }
