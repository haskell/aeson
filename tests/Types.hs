{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Types (module Types) where

import Prelude.Compat

import Math.NumberTheory.Logarithms (intLog2)
import Data.Data
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Hashable (Hashable (..))
import Data.Kind
#if !MIN_VERSION_base(4,16,0)
import Data.Semigroup (Option)
#endif
import Data.Text (Text)
import Data.Time (Day (..), fromGregorian)
import GHC.Generics
import Test.QuickCheck (Arbitrary (..), Property, counterexample, scale)
import Test.QuickCheck.Gen (chooseUpTo)
import qualified Data.Map as Map
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Word (Word64)

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

data NoConstructors

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

newtype MaybeField = MaybeField { maybeField :: Maybe Int }
#if !MIN_VERSION_base(4,16,0)
newtype OptionField = OptionField { optionField :: Option Int }
  deriving (Eq, Show)
#endif

deriving instance Generic Foo
deriving instance Generic UFoo
deriving instance Generic NoConstructors
deriving instance Generic OneConstructor
deriving instance Generic (Product2 a b)
deriving instance Generic (Product6 a b c d e f)
deriving instance Generic (Sum4 a b c d)
deriving instance Generic (Approx a)
deriving instance Generic Nullary
deriving instance Generic (SomeType a)
deriving instance Generic1 SomeType
#if !MIN_VERSION_base(4,16,0)
deriving instance Generic OptionField
#endif
deriving instance Generic EitherTextInt

failure :: Show a => String -> String -> a -> Property
failure func msg v = counterexample
                     (func ++ " failed: " ++ msg ++ ", " ++ show v) False

newtype BCEDay = BCEDay Day
  deriving (Eq, Show)

zeroDay :: Day
zeroDay = fromGregorian 0 0 0

instance Arbitrary BCEDay where
    arbitrary = fmap (BCEDay . ModifiedJulianDay . (+ toModifiedJulianDay zeroDay)) arbitrary

instance ToJSON BCEDay where
    toJSON (BCEDay d)     = toJSON d
    toEncoding (BCEDay d) = toEncoding d

instance FromJSON BCEDay where
    parseJSON = fmap BCEDay . parseJSON

-- | Scale the size of Arbitrary with ''
newtype LogScaled a = LogScaled { getLogScaled :: a }
  deriving (Eq, Ord, Show)

instance Hashable a => Hashable (LogScaled a) where
    hashWithSalt salt (LogScaled a) = hashWithSalt salt a

instance Arbitrary a => Arbitrary (LogScaled a) where
    arbitrary = LogScaled <$> scale (\x -> intLog2 $ x + 1) arbitrary
    shrink = fmap LogScaled . shrink . getLogScaled

instance ToJSON a => ToJSON (LogScaled a) where
    toJSON (LogScaled d)     = toJSON d
    toEncoding (LogScaled d) = toEncoding d

instance FromJSON a => FromJSON (LogScaled a) where
    parseJSON = fmap LogScaled . parseJSON

instance (ToJSONKey a) => ToJSONKey (LogScaled a) where
    toJSONKey = contramapToJSONKeyFunction getLogScaled toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (fmap getLogScaled) toJSONKeyList

instance (FromJSONKey a) => FromJSONKey (LogScaled a) where
    fromJSONKey = fmap LogScaled fromJSONKey
    fromJSONKeyList = coerceFromJSONKeyFunction (fromJSONKeyList :: FromJSONKeyFunction [a])

newtype UniformWord64 = U64 Word64
  deriving (Eq, Ord)

instance Show UniformWord64 where
    showsPrec d (U64 w) = showsPrec d w

instance Arbitrary UniformWord64 where
    arbitrary = U64 <$> chooseUpTo maxBound
    shrink (U64 w) = map U64 (shrink w)

-- NB: it should not have JSON instances
data Unit = Unit

data WithPhantom a = WithPhantom Int

deriveJSON defaultOptions ''WithPhantom

withPhantom :: WithPhantom Unit -> Value
withPhantom = toJSON

type family SomeTypeFamily a where
  SomeTypeFamily Unit = Int
  SomeTypeFamily ool = Unit

data WithTypeFamily a = WithTypeFamily (SomeTypeFamily a)

deriveJSON defaultOptions ''WithTypeFamily

withTypeFamilyUnit :: WithTypeFamily Unit -> Value
withTypeFamilyUnit = toJSON

withTypeFamilyA :: (ToJSON (SomeTypeFamily a)) => WithTypeFamily a -> Value
withTypeFamilyA = toJSON

-- -- No instance for ‘ToJSON Unit’
-- withTypeFamilyBool :: WithTypeFamily Bool -> Value
-- withTypeFamilyBool = toJSON

-- -- No instance for ‘ToJSON (SomeTypeFamily a)’
-- withTypeFamilyA' :: WithTypeFamily a -> Value
-- withTypeFamilyA' = toJSON

type family SomeTypeFamily1 a :: Type -> Type where
  SomeTypeFamily1 Unit = Identity

data SomeFunctor a = SomeFunctor a

deriveJSON1 defaultOptions ''SomeFunctor

data WithTypeFamily1 a b = WithTypeFamily1
  { a :: SomeTypeFamily1 a b
  , b :: SomeFunctor b
  }

deriveJSON1 defaultOptions ''WithTypeFamily1

withTypeFamily1Unit
  :: (ToJSON1 (SomeTypeFamily1 a))
  => (b -> Bool) -> (b -> Value) -> ([b] -> Value) -> WithTypeFamily1 a b -> Value
withTypeFamily1Unit = liftToJSON

data Lifted2 f a b = ManyLifted
  { lifted2 :: f a b
  }

deriveJSON2 defaultOptions ''Lifted2

data Lifted2Flipped f a b = Lifted2Flipped
  { lifted2Flipped :: f b a
  }

deriveJSON2 defaultOptions ''Lifted2Flipped

data Lifted1From2 f a b = Lifted1From2
  { lifted1From2 :: f a
  , lifted1From2' :: f b
  }

deriveJSON2 defaultOptions ''Lifted1From2

data Bar f g h a b = Bar
    { f :: f a b
    , g :: g b
    , h :: h Int
    }
deriveToJSON2 defaultOptions ''Bar
