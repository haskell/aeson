{-# LANGUAGE DerivingVia, GADTs, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module UnitTests.FromJSONKey (fromJSONKeyTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, Assertion, assertFailure, (@?=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import Data.Text (Text)
import Data.Tagged (Tagged)
import Control.Applicative (Const)

import Data.Aeson

newtype MyText = MyText Text
    deriving (FromJSONKey)

newtype MyText' = MyText' Text

instance FromJSONKey MyText' where
    fromJSONKey = fmap MyText' fromJSONKey
    fromJSONKeyList = error "not used"

newtype DownText = DownText Text
  deriving (Eq, Ord) via (Down Text)
  deriving FromJSON via Text

-- Regression test for #1169: don't rewrite fmap coerce to coerce
instance FromJSONKey DownText where
  fromJSONKey = fmap w fromJSONKey
    where w = DownText ; {-# NOINLINE w #-}

fromJSONKeyTests :: TestTree
fromJSONKeyTests = testGroup "FromJSONKey" $ fmap (testCase "-") fromJSONKeyAssertions

fromJSONKeyAssertions :: [Assertion]
fromJSONKeyAssertions =
    [ assertIsCoerce  "Text"            (fromJSONKey :: FromJSONKeyFunction Text)
    , assertIsCoerce  "Tagged Int Text" (fromJSONKey :: FromJSONKeyFunction (Tagged Int Text))
    , assertIsCoerce  "MyText"          (fromJSONKey :: FromJSONKeyFunction MyText)

    , assertIsText    "MyText'"         (fromJSONKey :: FromJSONKeyFunction MyText')
    , assertIsCoerce  "Const Text"      (fromJSONKey :: FromJSONKeyFunction (Const Text ()))

    , assertDecodedMapIsValid
    ]
  where
    assertIsCoerce :: String -> FromJSONKeyFunction a -> Assertion
    assertIsCoerce _ FromJSONKeyCoerce = pure ()
    assertIsCoerce n _                 = assertFailure n

    assertIsText :: String -> FromJSONKeyFunction a -> Assertion
    assertIsText _ (FromJSONKeyText _) = pure ()
    assertIsText n _               = assertFailure n

-- Regression test for #1169 (see FromJSONKey DownText)
assertDecodedMapIsValid :: Assertion
assertDecodedMapIsValid = fmap Map.valid decodedMap @?= Just True
  where
    decodedMap = decode "{\"a\":\"a\",\"b\":\"b\"}" :: Maybe (Map DownText Text)
