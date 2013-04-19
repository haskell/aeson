{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import qualified Data.Map as Map
import Data.Data
import Data.Text

data Foo = Foo {
      fooInt :: Int
    , fooDouble :: Double
    , fooTuple :: (String, Text, Int)
    -- This definition causes an infinite loop in genericTo and genericFrom!
    -- , fooMap :: Map.Map String Foo
    , fooMap :: Map.Map String (Text,Int)
    } deriving (Show, Typeable, Data)

data UFoo = UFoo {
      _UFooInt :: Int
    , uFooInt :: Int
    } deriving (Show, Eq, Data, Typeable)
