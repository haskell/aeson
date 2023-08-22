{-# LANGUAGE OverloadedStrings, DeriveGeneric  #-}
module Regression.Issue571 (issue571) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import GHC.Generics (Generic)

import Data.Aeson

data F = F
    { a :: Maybe Int
    , b :: Maybe Int
    , c :: ()
    }
  deriving (Eq, Show, Generic)

instance FromJSON F where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = False } -- default

data G = G
    { e :: Maybe Int
    , f :: Maybe Int
    , g :: ()
    }
  deriving (Eq, Show, Generic)

instance FromJSON G where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = False, allowOmittedFields = False }


issue571 :: TestTree
issue571 = testCase "issue571" $ do
    -- the Maybe fields can be omitted.
    let actualF = decode "{}" :: Maybe F
    actualF @?= Just F { a = Nothing, b = Nothing, c = () }

    (decode "{}" :: Maybe G) @?= Nothing
    (decode "{\"e\":1, \"f\":2}" :: Maybe G) @?= Nothing
    (decode "{\"e\":1, \"g\":[]}" :: Maybe G) @?= Nothing
    (decode "{\"f\":2, \"g\":[]}" :: Maybe G) @?= Nothing
    (decode "{\"e\":1, \"f\":2, \"g\":[]}"   :: Maybe G) @?= Just G { e = Just 1, f = Just 2, g = () }
    (decode "{\"e\":1, \"f\":2, \"g\":true}" :: Maybe G) @?= Just G { e = Just 1, f = Just 2, g = () }
