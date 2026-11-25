{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson.Parser (json, json')
import Data.Aeson.Types (Value (..))
import Data.Either (isLeft)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = defaultMain $ testGroup "attoparsec-aeson" [controlCharacters]

parse :: A.Parser Value -> BC.ByteString -> Either String Value
parse p = A.parseOnly (p <* A.endOfInput)

-- | Strings with an escape or a non-ASCII character take a different path
-- from plain ASCII strings, which must reject control characters too.
controlCharacters :: TestTree
controlCharacters = testGroup "Unescaped control characters"
  [ testGroup "rejected"
    [ testCase (show s) $ do
        assertBool "json" (isLeft (parse json s))
        assertBool "json'" (isLeft (parse json' s))
    | s <- [ "\"\t\""
           , "\"\\\"\t\""
           , "\"\\n\NUL\""
           , "\"\195\169\t\""
           , "[\"\\u0041\n\"]"
           , "{\"\195\169\t\":1}"
           ]
    ]
  , testCase "escapes and non-ASCII characters are accepted" $
      parse json "\"\\t\195\169\"" @?= Right (String "\t\233")
  ]
