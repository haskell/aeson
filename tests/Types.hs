{-# LANGUAGE DefaultSignatures, DeriveDataTypeable, DeriveGeneric,
    GeneralizedNewtypeDeriving #-}

module Types where

import qualified Data.Map as Map
import Data.Data
import Data.Text
import GHC.Generics

data Foo = Foo {
      fooInt :: Int
    , fooDouble :: Double
    , fooTuple :: (String, Text, Int)
    -- This definition causes an infinite loop in genericTo and genericFrom!
    -- , fooMap :: Map.Map String Foo
    , fooMap :: Map.Map String (Text,Int)
    } deriving (Show, Typeable, Data, Generic)

data UFoo = UFoo {
      _UFooInt :: Int
    , uFooInt :: Int
    } deriving (Show, Eq, Data, Typeable, Generic)

data OneConstructor = OneConstructor
                      deriving (Show, Eq, Typeable, Data, Generic)

data Product2 a b = Product2 a b
                    deriving (Show, Eq, Typeable, Data, Generic)

data Product6 a b c d e f = Product6 a b c d e f
                    deriving (Show, Eq, Typeable, Data, Generic)

data Sum4 a b c d = Alt1 a | Alt2 b | Alt3 c | Alt4 d
                    deriving (Show, Eq, Typeable, Data, Generic)

class ApproxEq a where
    (=~) :: a -> a -> Bool

    default (=~) :: (Eq a) => a -> a -> Bool
    (=~) = (==)

newtype Approx a = Approx { fromApprox :: a }
    deriving (Show, Data, Typeable, Generic, ApproxEq, Num)

instance (ApproxEq a) => Eq (Approx a) where
    Approx a == Approx b = a =~ b
