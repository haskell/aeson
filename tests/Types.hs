{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Prelude ()
import Prelude.Compat

import Data.Data
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Text
import GHC.Generics
import Test.QuickCheck (Property, counterexample)
import qualified Data.Map as Map

type I = Identity
type Compose3  f g h = Compose (Compose f g) h
type Compose3' f g h = Compose f (Compose g h)

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

data OneConstructor = OneConstructor
                      deriving (Show, Eq, Typeable, Data)

data Product2 a b = Product2 a b
                    deriving (Show, Eq, Typeable, Data)

data Product6 a b c d e f = Product6 a b c d e f
                    deriving (Show, Eq, Typeable, Data)

data Sum4 a b c d = Alt1 a | Alt2 b | Alt3 c | Alt4 d
                    deriving (Show, Eq, Typeable, Data)

class ApproxEq a where
    (=~) :: a -> a -> Bool

newtype Approx a = Approx { fromApprox :: a }
    deriving (Show, Data, Typeable, ApproxEq, Num)

instance (ApproxEq a) => Eq (Approx a) where
    Approx a == Approx b = a =~ b

data Nullary = C1 | C2 | C3 deriving (Eq, Show)

data SomeType a = Nullary
                | Unary Int
                | Product String (Maybe Char) a
                | Record { testOne   :: Double
                         , testTwo   :: Maybe Bool
                         , testThree :: Maybe a
                         }
                | List [a]
  deriving (Eq, Show)

-- | This type requires IncoherentInstances for the instances of the type
-- classes Data.Aeson.TH.LookupField and Data.Aeson.Types.FromJSON.FromRecord.
--
-- The minimum known requirements for this type are:
-- * Record type with at least two fields
-- * One field type is either a type parameter or a type/data family
-- * Another field type is a @Maybe@ of the above field type
data IncoherentInstancesNeeded a = IncoherentInstancesNeeded
  { incoherentInstancesNeededMaybeNot :: a
  , incoherentInstancesNeededMaybeYes :: Maybe a
  } deriving Generic

-- Used for testing UntaggedValue SumEncoding
data EitherTextInt
    = LeftBool Bool
    | RightInt Int
    | BothTextInt Text Int
    | NoneNullary
    deriving (Eq, Show)

data GADT a where
    GADT :: { gadt :: String } -> GADT String
  deriving Typeable

deriving instance Data (GADT String)
deriving instance Eq   (GADT a)
deriving instance Show (GADT a)

deriving instance Generic Foo
deriving instance Generic UFoo
deriving instance Generic OneConstructor
deriving instance Generic (Product2 a b)
deriving instance Generic (Product6 a b c d e f)
deriving instance Generic (Sum4 a b c d)
deriving instance Generic (Approx a)
deriving instance Generic Nullary
deriving instance Generic (SomeType a)
#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic1 SomeType
#endif
deriving instance Generic EitherTextInt

failure :: Show a => String -> String -> a -> Property
failure func msg v = counterexample
                     (func ++ " failed: " ++ msg ++ ", " ++ show v) False
