{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module UnitTests.MonadFix (monadFixTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))

import qualified Data.Map as Map -- Lazy

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.KeyMap as KM

-------------------------------------------------------------------------------
-- MonadFix
-------------------------------------------------------------------------------

monadFixDecoding1 :: (Value -> Data.Aeson.Types.Parser [Char]) -> Assertion
monadFixDecoding1 p = do
    fmap (take 10) (parseMaybe p value) @?= Just "xyzxyzxyzx"
  where
    value = object
        [ "foo" .= ('x', "bar" :: String)
        , "bar" .= ('y', "quu" :: String)
        , "quu" .= ('z', "foo" :: String)
        ]

monadFixDecoding2 :: (Value -> Data.Aeson.Types.Parser [Char]) -> Assertion
monadFixDecoding2 p = do
    fmap (take 10) (parseMaybe p value) @?= Nothing
  where
    value = object
        [ "foo" .= ('x', "bar" :: String)
        , "bar" .= ('y', "???" :: String)
        , "quu" .= ('z', "foo" :: String)
        ]

monadFixDecoding3 :: (Value -> Data.Aeson.Types.Parser [Char]) -> Assertion
monadFixDecoding3 p =
    fmap (take 10) (parseMaybe p value) @?= Nothing
  where
    value = object
        [ "foo" .= ('x', "bar" :: String)
        , "bar" .= Null
        , "quu" .= ('z', "foo" :: String)
        ]

monadFixDecoding4 :: (Value -> Data.Aeson.Types.Parser [Char]) -> Assertion
monadFixDecoding4 p =
    fmap (take 10) (parseMaybe p value) @?= Nothing
  where
    value = object
        [ "els" .= ('x', "bar" :: String)
        , "bar" .= Null
        , "quu" .= ('z', "foo" :: String)
        ]

-- Parser with explicit references
monadFixParserA :: Value -> Data.Aeson.Types.Parser [Char]
monadFixParserA = withObject "Rec" $ \obj -> mdo
    let p'' :: Value -> Data.Aeson.Types.Parser String
        p'' "foo" = return foo
        p'' "bar" = return bar
        p'' "quu" = return quu
        p'' _     = fail "Invalid reference"

    let p' :: Value -> Data.Aeson.Types.Parser [Char]
        p' v = do
            (c, cs) <- liftParseJSON Nothing p'' (listParser p'') v
            return (c : cs)

    foo <- explicitParseField p' obj "foo"
    bar <- explicitParseField p' obj "bar"
    quu <- explicitParseField p' obj "quu"
    return foo

-- Parser with arbitrary references!
monadFixParserB :: Value -> Data.Aeson.Types.Parser [Char]
monadFixParserB = withObject "Rec" $ \obj -> mdo
    let p'' :: Value -> Data.Aeson.Types.Parser String
        p'' key' = do
            key <- parseJSON key'
            -- this is ugly: we look whether key is in original obj
            -- but then query from refs.
            --
            -- This way we are lazier. Map.traverse isn't lazy enough.
            case KM.lookup key obj of
                Just _  -> return (refs Map.! key)
                Nothing -> fail "Invalid reference"

    let p' :: Value -> Data.Aeson.Types.Parser [Char]
        p' v = do
            (c, cs) <- liftParseJSON Nothing p'' (listParser p'') v
            return (c : cs)

    refs <- traverse p' (KM.toMap obj)
    case Map.lookup "foo" refs of
        Nothing   -> fail "No foo node"
        Just root -> return root

monadFixTests :: TestTree
monadFixTests = testGroup "MonadFix"
    [ testCase "Example1a" $ monadFixDecoding1 monadFixParserA
    , testCase "Example2a" $ monadFixDecoding2 monadFixParserA
    , testCase "Example3a" $ monadFixDecoding3 monadFixParserA
    , testCase "Example4a" $ monadFixDecoding4 monadFixParserA

    , testCase "Example1b" $ monadFixDecoding1 monadFixParserB
    , testCase "Example2b" $ monadFixDecoding2 monadFixParserB
    , testCase "Example3b" $ monadFixDecoding3 monadFixParserB
    , testCase "Example4b" $ monadFixDecoding4 monadFixParserB
    ]
