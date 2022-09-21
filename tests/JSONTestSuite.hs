module JSONTestSuite (tests) where

import Test.Tasty (TestTree, testGroup)
import Data.Either.Compat (isLeft, isRight)
import Test.Tasty.HUnit ( testCase, assertBool )
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), takeExtension, takeFileName)
import Data.List (sort)
import Control.Monad (forM)

import qualified Data.ByteString.Lazy as L
import qualified Data.HashSet as HashSet

import Data.Aeson

jsonTestSuiteTest :: FilePath -> TestTree
jsonTestSuiteTest path = testCase fileName $ do
    payload <- L.readFile path
    let result = eitherDecode payload :: Either String Value
    assertBool (show result) $ case take 2 fileName of
      "i_" -> isRight result
      "n_" -> isLeft result
      "y_" -> isRight result
      _    -> isRight result -- test_transform tests have inconsistent names
  where
    fileName = takeFileName path

-- Build a collection of tests based on the current contents of the
-- JSONTestSuite test directories.

tests :: IO TestTree
tests = do
  let suitePath = "tests/JSONTestSuite"
  let suites = ["test_parsing", "test_transform"]
  testPaths <- fmap (sort . concat) . forM suites $ \suite -> do
    let dir = suitePath </> suite
    entries <- getDirectoryContents dir
    let ok name = takeExtension name == ".json" &&
                  not (name `HashSet.member` blacklist)
    return . map (dir </>) . filter ok $ entries
  return $ testGroup "JSONTestSuite" $ map jsonTestSuiteTest testPaths

-- The set expected-to-be-failing JSONTestSuite tests.
-- Not all of these failures are genuine bugs.
-- Of those that are bugs, not all are worth fixing.

blacklist :: HashSet.HashSet String
-- blacklist = HashSet.empty
blacklist = _blacklist

_blacklist :: HashSet.HashSet String
_blacklist = HashSet.fromList
  [ "i_string_UTF8_surrogate_U+D800.json"
  , "i_object_key_lone_2nd_surrogate.json"
  , "i_string_1st_surrogate_but_2nd_missing.json"
  , "i_string_1st_valid_surrogate_2nd_invalid.json"
  , "i_string_UTF-16LE_with_BOM.json"
  , "i_string_UTF-16_invalid_lonely_surrogate.json"
  , "i_string_UTF-16_invalid_surrogate.json"
  , "i_string_UTF-8_invalid_sequence.json"
  , "i_string_incomplete_surrogate_and_escape_valid.json"
  , "i_string_incomplete_surrogate_pair.json"
  , "i_string_incomplete_surrogates_escape_valid.json"
  , "i_string_invalid_lonely_surrogate.json"
  , "i_string_invalid_surrogate.json"
  , "i_string_inverted_surrogates_U+1D11E.json"
  , "i_string_lone_second_surrogate.json"
  , "i_string_not_in_unicode_range.json"
  , "i_string_truncated-utf-8.json"
  , "i_structure_UTF-8_BOM_empty_object.json"
  , "string_1_escaped_invalid_codepoint.json"
  , "string_1_invalid_codepoint.json"
  , "string_1_invalid_codepoints.json"
  , "string_2_escaped_invalid_codepoints.json"
  , "string_2_invalid_codepoints.json"
  , "string_3_escaped_invalid_codepoints.json"
  , "string_3_invalid_codepoints.json"
  , "y_string_utf16BE_no_BOM.json"
  , "y_string_utf16LE_no_BOM.json"
  ]