{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErrorMessages
  (
    tests
  ) where

import Prelude ()
import Prelude.Compat

import Data.Aeson (FromJSON(..), eitherDecode)
import Data.Proxy (Proxy(..))
import Instances ()
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertEqual)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as HM

tests :: [Test]
tests =
    [
      testCase "Int" int
    , testCase "String" string
    , testCase "HashMap" hashMap
    ]

int :: Assertion
int = do
  let t = test (Proxy :: Proxy Int)
  t "\"\"" $ expected "Int" "String"
  t "[]" $ expected "Int" "Array"
  t "{}" $ expected "Int" "Object"
  t "null" $ expected "Int" "Null"

string :: Assertion
string = do
  let t = test (Proxy :: Proxy String)
  t "1" $ expected "String" "Number"
  t "[]" $ expected "String" "Array"
  t "{}" $ expected "String" "Object"
  t "null" $ expected "String" "Null"

hashMap :: Assertion
hashMap = do
  let t = test (Proxy :: Proxy (HM.HashMap String Int))
  t "\"\"" $ expected "HashMap k v" "String"
  t "[]" $ expected "HashMap k v" "Array"

expected :: String -> String -> String
expected ex enc = "Error in $: expected " ++ ex ++ ", encountered " ++ enc

test :: forall a proxy . (FromJSON a, Show a) => proxy a -> L.ByteString -> String -> Assertion
test _ v msg = case eitherDecode v of
    Left e -> assertEqual "Invalid error message" msg e
    Right (x :: a) -> assertFailure $ "Expected parsing to fail but it suceeded with: " ++ show x
