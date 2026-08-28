{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module UnitTests.FromJSONKey (fromJSONKeyTests) where

import Control.Applicative (Const)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Ord (Down(Down))
import Data.Tagged (Tagged)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

newtype MyText = MyText Text
    deriving (FromJSONKey)

newtype MyText' = MyText' Text
    deriving FromJSON via Text

instance FromJSONKey MyText' where
    fromJSONKey = fmap MyText' fromJSONKey

newtype ReverseText = ReverseText Text
    deriving (Eq, Ord) via Down Text
    deriving FromJSON via Text

instance FromJSONKey ReverseText where
    fromJSONKey = fmap ReverseText fromJSONKey

newtype FoldedText = FoldedText Text
    deriving (Eq, Ord, Show) via Text
    deriving FromJSON via Text

instance FromJSONKey FoldedText where
    fromJSONKey = fmap (FoldedText . Text.toCaseFold) fromJSONKey

fromJSONKeyTests :: TestTree
fromJSONKeyTests = testGroup "FromJSONKey"
    [ testGroup "strategies" $ fmap (testCase "-") fromJSONKeyAssertions
    , testCase "fmap decoding preserves Map ordering" assertDecodedMapIsValid
    , testCase "fmap decoding rebuilds colliding Map keys" assertDecodedCollidingMap
    ]

fromJSONKeyAssertions :: [Assertion]
fromJSONKeyAssertions =
    [ assertIsCoerce  "Text"            (fromJSONKey :: FromJSONKeyFunction Text)
    , assertIsCoerce  "Tagged Int Text" (fromJSONKey :: FromJSONKeyFunction (Tagged Int Text))
    , assertIsCoerce  "MyText"          (fromJSONKey :: FromJSONKeyFunction MyText)

    , assertIsText    "MyText'"         (fromJSONKey :: FromJSONKeyFunction MyText')
    , assertIsCoerce  "Const Text"      (fromJSONKey :: FromJSONKeyFunction (Const Text ()))
    ]
  where
    assertIsCoerce :: String -> FromJSONKeyFunction a -> Assertion
    assertIsCoerce _ FromJSONKeyCoerce = pure ()
    assertIsCoerce n _                 = assertFailure n

    assertIsText :: String -> FromJSONKeyFunction a -> Assertion
    assertIsText _ (FromJSONKeyText _) = pure ()
    assertIsText n _                   = assertFailure n

assertDecodedMapIsValid :: Assertion
assertDecodedMapIsValid = fmap Map.valid decodedMap @?= Just True
  where
    decodedMap = decode "{\"a\":\"a\",\"b\":\"b\"}" :: Maybe (Map ReverseText Text)

assertDecodedCollidingMap :: Assertion
assertDecodedCollidingMap = decodedMap @?= Just (Map.singleton (FoldedText "a") "upper")
  where
    decodedMap = decode "{\"A\":\"upper\",\"a\":\"lower\"}" :: Maybe (Map FoldedText Text)
