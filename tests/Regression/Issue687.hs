{-# LANGUAGE DeriveGeneric, TypeApplications, OverloadedStrings, TemplateHaskell, DuplicateRecordFields #-}
module Regression.Issue687 where

import GHC.Generics (Generic1)
import Data.Aeson
import Data.Aeson.Types (iparseEither)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.TH (deriveJSON1)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

data ExG a = ExG { required :: a, optional :: Maybe a }
    deriving (Eq, Show, Generic1)

data ExTH a = ExTH { required :: a, optional :: Maybe a }
    deriving (Eq, Show, Generic1)

instance ToJSON1 ExG where
    liftToJSON     = genericLiftToJSON     defaultOptions { omitNothingFields = True }
    liftToEncoding = genericLiftToEncoding defaultOptions { omitNothingFields = True }

instance FromJSON1 ExG where
    liftParseJSON  = genericLiftParseJSON  defaultOptions { omitNothingFields = True }

$(deriveJSON1 defaultOptions { omitNothingFields = True } ''ExTH)

issue687 :: TestTree
issue687 = testCase "issue687" $ do
  example (ExG @Int 1 Nothing)  $ object [ "required" .= (1 :: Int) ]
  example (ExG @Int 1 (Just 2)) $ object [ "required" .= (1 :: Int), "optional" .= (2 :: Int) ]

  example (ExTH @Int 1 Nothing)  $ object [ "required" .= (1 :: Int) ]
  example (ExTH @Int 1 (Just 2)) $ object [ "required" .= (1 :: Int), "optional" .= (2 :: Int) ]

  where
    example :: (ToJSON1 f, FromJSON1 f, Eq (f Int), Show (f Int)) => f Int -> Value -> IO ()
    example x val = do
        -- encoding
        toJSON1 x @?= val
        decode (encodingToLazyByteString (toEncoding1 x)) @?= Just val

        -- decoding
        iparseEither parseJSON1 val @?= Right x
