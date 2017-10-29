{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module UnitTests.AccErrors (tests) where

import Prelude ()
import Prelude.Compat hiding (seq)

import Data.Aeson
import Data.Aeson.Parser.Internal
import Data.Aeson.Types ()
import Data.Aeson.Internal
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup
import Data.Vector (Vector)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import qualified Data.ByteString.Lazy as L
import qualified Data.List.NonEmpty as NL
import qualified Data.Sequence as Seq

tests :: Test
tests = testGroup "Error accumulation" [
      testCase "seq" seq
    , testCase "vector" vector
    ]

decoder :: FromJSON a
    => L.ByteString
    -> Either (NonEmpty (JSONPath, String)) a
decoder = verboseDecodeWith jsonEOF ifromJSON

seq :: Assertion
seq = do
    let res = decoder "[true, null]" :: Either (NonEmpty (JSONPath, String)) (Seq.Seq Int)
    let message i s = ([Index i], "expected Int, encountered " <> s)
    res @=? Left (NL.fromList [message 0 "Boolean", message 1 "Null"])

vector :: Assertion
vector = do
    let res = decoder "[true, null]" :: Either (NonEmpty (JSONPath, String)) (Vector Int)
    let message i s = ([Index i], "expected Int, encountered " <> s)
    res @=? Left (NL.fromList [message 0 "Boolean", message 1 "Null"])
