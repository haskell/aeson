{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module UnitTests.NoThunks where

import           Test.Tasty            (TestTree, testGroup)

#if __GLASGOW_HASKELL__ >=902 && __GLASGOW_HASKELL__ <907

import           Data.Maybe            (isNothing)
import           NoThunks.Class        (NoThunks (..), allNoThunks, noThunksInKeysAndValues)
import           Test.QuickCheck       (ioProperty)
import           Test.Tasty.HUnit      (assertFailure, testCase)
import           Test.Tasty.QuickCheck (testProperty)

import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.Scientific       as Sci

import           Data.Aeson

noThunksTests :: TestTree
noThunksTests = testGroup "nothunks"
    [ testNoThunks "example1" "null"
    , testNoThunks "example2" "[ 1, 2, 3, true ]"
    , testNoThunks "example3" "{ \"1\": 1, \"2\": 2 }"
    , testProperty "property" $ \input -> ioProperty $ do
        let lbs = encode (input :: Value)
        !value <- either fail return $ eitherDecode lbs
        isNothing <$> noThunks [] (value :: Value)
    ]
  where
    testNoThunks name bs = testCase name $ do
        !value <- either fail return $ eitherDecode bs
        x <- noThunks [] (value :: Value)
        case x of
            Nothing -> return ()
            Just ti -> assertFailure $ show ti

instance NoThunks Value

instance NoThunks v => NoThunks (KM.KeyMap v) where
    wNoThunks ctx m = noThunksInKeysAndValues ctx (KM.toList m)
    showTypeOf _ = "KeyMap"

instance NoThunks K.Key where
    wNoThunks _ _ = return Nothing
    showTypeOf _ = "Key"

instance NoThunks Sci.Scientific where
    wNoThunks ctx s = do
        let !c = Sci.coefficient s
        let !e = Sci.base10Exponent s
        allNoThunks [ wNoThunks ctx c, wNoThunks ctx e ]
    showTypeOf _ = "Scientific"

#else

-- for other GHCs the test group is empty
noThunksTests :: TestTree
noThunksTests = testGroup "nothunks" []

#endif
