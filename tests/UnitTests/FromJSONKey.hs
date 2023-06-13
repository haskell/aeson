{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UnitTests.FromJSONKey (fromJSONKeyTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, Assertion, assertFailure)
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

fromJSONKeyTests :: TestTree
fromJSONKeyTests = testGroup "FromJSONKey" $ fmap (testCase "-") fromJSONKeyAssertions

fromJSONKeyAssertions :: [Assertion]
fromJSONKeyAssertions =
    [ assertIsCoerce  "Text"            (fromJSONKey :: FromJSONKeyFunction Text)
    , assertIsCoerce  "Tagged Int Text" (fromJSONKey :: FromJSONKeyFunction (Tagged Int Text))
    , assertIsCoerce  "MyText"          (fromJSONKey :: FromJSONKeyFunction MyText)

    , assertIsCoerce' "MyText'"         (fromJSONKey :: FromJSONKeyFunction MyText')
    , assertIsCoerce  "Const Text"      (fromJSONKey :: FromJSONKeyFunction (Const Text ()))
    ]
  where
    assertIsCoerce :: String -> FromJSONKeyFunction a -> Assertion
    assertIsCoerce _ FromJSONKeyCoerce = pure ()
    assertIsCoerce n _                 = assertFailure n

    assertIsCoerce' :: String -> FromJSONKeyFunction a -> Assertion
    assertIsCoerce' _ FromJSONKeyCoerce = pure ()
    assertIsCoerce' n _                 = pickWithRules (assertFailure n) (pure ())

-- | Pick the first when RULES are enabled, e.g. optimisations are on
pickWithRules
    :: a -- ^ Pick this when RULES are on
    -> a -- ^ use this otherwise
    -> a
pickWithRules _ = id
{-# NOINLINE pickWithRules #-}
{-# RULES "pickWithRules/rule" [0] forall x. pickWithRules x = const x #-}
