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
      "n_"                                                -> isLeft result
      "y_"                                                -> isRight result
      "i_" | fileName `HashSet.member` ignore_accepted    -> isRight result
           | otherwise                                    -> isLeft result
      _    | fileName `HashSet.member` transform_rejected -> isLeft result
           | otherwise                                    -> isRight result -- test_transform tests have inconsistent names
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
    let ok name = takeExtension name == ".json"
    return . map (dir </>) . filter ok $ entries
  return $ testGroup "JSONTestSuite" $ map jsonTestSuiteTest testPaths

-- The set expected-to-be-failing JSONTestSuite tests.
-- Not all of these failures are genuine bugs.
-- Of those that are bugs, not all are worth fixing.

-- | The @i@ cases we can ignore. We don't.
--
-- @i_@ - parsers are free to accept or reject content
--
-- We specify which @i_@ case we accept, so we can catch changes even in unspecified behavior.
-- (There is less case we accept)
ignore_accepted :: HashSet.HashSet FilePath
ignore_accepted = HashSet.fromList
    [ "i_number_double_huge_neg_exp.json"
    , "i_number_huge_exp.json"
    , "i_number_neg_int_huge_exp.json"
    , "i_number_pos_double_huge_exp.json"
    , "i_number_real_neg_overflow.json"
    , "i_number_real_pos_overflow.json"
    , "i_number_real_underflow.json"
    , "i_number_too_big_neg_int.json"
    , "i_number_too_big_pos_int.json"
    , "i_number_very_big_negative_int.json"
    , "i_structure_500_nested_arrays.json"
    ]

-- | Transform folder contain weird structures and characters that parsers may understand differently.
--
-- We don't even try to understand some.
transform_rejected :: HashSet.HashSet FilePath
transform_rejected = HashSet.fromList
    [ "string_1_escaped_invalid_codepoint.json"
    , "string_1_invalid_codepoint.json"
    , "string_2_escaped_invalid_codepoints.json"
    , "string_2_invalid_codepoints.json"
    , "string_3_escaped_invalid_codepoints.json"
    , "string_3_invalid_codepoints.json"
    ]