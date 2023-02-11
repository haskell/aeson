module JSONTestSuite (tests) where

import Test.Tasty (TestTree, testGroup)
import Data.Either.Compat (isLeft, isRight)
import Test.Tasty.HUnit ( testCase, assertBool )
import Test.Tasty.Golden (goldenVsStringDiff)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), (-<.>), takeExtension, takeFileName)
import Data.List (sort)
import Control.Monad (forM)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.HashSet as HashSet

import Data.Aeson
import qualified Data.Aeson.Decoding as D
import Data.Aeson.Decoding.Tokens
import qualified Data.Aeson.Decoding.ByteString as D
import qualified Data.Aeson.Decoding.ByteString.Lazy as D


jsonTestSuiteTest :: FilePath -> TestTree
jsonTestSuiteTest path = case take 2 fileName of
    "n_"                                                -> negative
    "y_"                                                -> positive
    "i_" | fileName `HashSet.member` ignore_accepted    -> positive
         | otherwise                                    -> negative
    _    | fileName `HashSet.member` transform_rejected -> negative
         | otherwise                                    -> positive
  where
    fileName = takeFileName path

    diff ref new = ["diff", "-u", ref, new]

    positive = testGroup fileName
        [ goldenVsStringDiff "decode" diff ("tests" </>  "JSONTestSuite" </> "results" </> fileName -<.> "txt") $ do
            payload <- B.readFile path
            let result = eitherDecodeStrict payload :: Either String Value
            assertBool (show result) (isRight result)
            return (LBS8.pack (show result ++ "\n"))

        , goldenVsStringDiff "decode via tokens" diff ("tests" </>  "JSONTestSuite" </> "results" </> fileName -<.> "txt") $ do
            payload <- B.readFile path
            let result = D.eitherDecodeStrict payload :: Either String Value
            assertBool (show result) (isRight result)
            return (LBS8.pack (show result ++ "\n"))

        , goldenVsStringDiff "tokens bs" diff ("tests" </>  "JSONTestSuite" </> "results" </> fileName -<.> "tok") $ do
            payload <- B.readFile path
            let result = D.bsToTokens payload
            return (LBS8.pack (unlines (showTokens (const []) result)))

        , goldenVsStringDiff "tokens lbs" diff ("tests" </>  "JSONTestSuite" </> "results" </> fileName -<.> "tok") $ do
            payload <- L.readFile path
            let result = D.lbsToTokens payload
            return (LBS8.pack (unlines (showTokens (const []) result)))

        ]

    negative = testGroup fileName
        [ testCase "decode" $ do
            payload <- B.readFile path

            let result1 = eitherDecodeStrict payload :: Either String Value
            assertBool ("decode:" ++ show result1) (isLeft result1)

            let result2 = D.eitherDecodeStrict payload :: Either String Value
            assertBool ("strict:" ++ show result2) (isLeft result2)

            payloadL <- L.readFile path

            let result3 = D.eitherDecode payloadL :: Either String Value
            assertBool ("lazy:" ++ show result3) (isLeft result3)

        , goldenVsStringDiff "tokens bs" diff ("tests" </>  "JSONTestSuite" </> "results" </> fileName -<.> "tok") $ do
            payload <- B.readFile path
            let result = D.bsToTokens payload
            return (LBS8.pack (unlines (showTokens (const []) result)))

        , goldenVsStringDiff "tokens lbs" diff ("tests" </>  "JSONTestSuite" </> "results" </> fileName -<.> "tok") $ do
            payload <- L.readFile path
            let result = D.lbsToTokens payload
            return (LBS8.pack (unlines (showTokens (const []) result)))

        ]

showTokens :: Show e => (k -> [String]) -> Tokens k e -> [String]
showTokens kont (TkLit l k)      = ("TkLit " ++ show l) : kont k
showTokens kont (TkText t k)     = ("TkText " ++ show t) : kont k
showTokens kont (TkNumber n k)   = ("TkNumber " ++ show n) : kont k
showTokens kont (TkArrayOpen k)  = "TkArrayOpen" : showTkArray kont k
showTokens kont (TkRecordOpen k) = "TkRecordOpen" : showTkRecord kont k
showTokens _    (TkErr e)        = ["TkErr " ++ show e]

showTkArray :: Show e => (k -> [String]) -> TkArray k e -> [String]
showTkArray kont (TkItem k)     = "TkItem" : showTokens (showTkArray kont) k
showTkArray kont (TkArrayEnd k) = "TkArrayEnd" : kont k
showTkArray _    (TkArrayErr e) = ["TkArrayErr " ++ show e]

showTkRecord :: Show e => (k -> [String]) -> TkRecord k e -> [String]
showTkRecord kont (TkPair x k)    = ("TkPair " ++ show x) : showTokens (showTkRecord kont) k
showTkRecord kont (TkRecordEnd k) = "TkRecordEnd" : kont k
showTkRecord _    (TkRecordErr e) = ["TkRecordErr " ++ show e]


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
