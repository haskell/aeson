{-# LANGUAGE OverloadedStrings #-}
module Regression.Issue1198 (issue1198) where

import Data.Version (Version, makeVersion, parseVersion)
import Test.QuickCheck (Gen, elements, forAll, listOf, (===))
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types (parseMaybe)

-- | The previous implementation.
viaReadP :: String -> Maybe Version
viaReadP s = case [v | (v, "") <- readP_to_S parseVersion s] of
    v : _ -> Just v
    []    -> Nothing

versionLike :: Gen String
versionLike = listOf (elements "0123456789..--abZ_ \x0663\x00e9")

issue1198 :: TestTree
issue1198 = testGroup "Issue #1198"
  [ testProperty "accepts the same strings as parseVersion" $ forAll versionLike $ \s ->
      parseMaybe parseJSON (String (T.pack s)) === viaReadP s
  , localOption (mkTimeout 5000000) $ testCase "long version" $ do
      let n = 100000
      assertEqual "" (Just (makeVersion (replicate n 1)))
        (parseMaybe parseJSON (String (T.intercalate "." (replicate n "1"))))
  ]
