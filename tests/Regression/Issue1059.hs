{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Regression.Issue1059 (issue1059) where

import GHC.Generics
import Data.Aeson
import Test.Tasty
import Test.Tasty.HUnit

data Item f a = Item { rec0 :: Int, par1 :: a, rec1 :: f a, comp1 :: f (f a) } deriving (Functor, Generic1)

deriving instance (Eq a, Eq (f a), Eq (f (f a))) => Eq (Item f a)
deriving instance (Show a, Show (f a), Show (f (f a))) => Show (Item f a)

instance (Functor f, FromJSON1 f) => FromJSON1 (Item f) where
    liftParseJSON = genericLiftParseJSON $ defaultOptions { allowOmittedFields = True }
instance (Functor f, ToJSON1 f) => ToJSON1 (Item f) where
    liftToJSON = genericLiftToJSON $ defaultOptions { omitNothingFields = True }
instance (Functor f, FromJSON1 f, FromJSON a) => FromJSON (Item f a) where parseJSON = parseJSON1
instance (Functor f, ToJSON1 f, ToJSON a) => ToJSON (Item f a) where toJSON = toJSON1

data Test a = Test { a :: Item [] (Maybe a), b :: Item Maybe a } deriving (Eq, Show, Generic1)

instance FromJSON1 Test where liftParseJSON = genericLiftParseJSON defaultOptions
instance ToJSON1 Test where liftToJSON = genericLiftToJSON defaultOptions
instance FromJSON a => FromJSON (Test a) where parseJSON = parseJSON1
instance ToJSON a => ToJSON (Test a) where toJSON = toJSON1

issue1059 :: TestTree
issue1059 = testCase "issue1059" $ do
    let value = Test (Item 0 Nothing [] []) (Item 0 1 Nothing Nothing) :: Test Int
    let code = "{\"a\":{\"comp1\":[],\"rec0\":0,\"rec1\":[]},\"b\":{\"par1\":1,\"rec0\":0}}"
    encode value @?= code
    decode code @?= Just value
