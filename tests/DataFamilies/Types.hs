{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module DataFamilies.Types where

import Types (ApproxEq(..))

data family Nullary a
data instance Nullary Int  = C1 | C2 | C3 deriving (Eq, Show)
data instance Nullary Char = C4           deriving (Eq, Show)

data family SomeType a b c
data instance SomeType c () a = Nullary
                              | Unary Int
                              | Product String (Maybe Char) a
                              | Record { testOne   :: Double
                                       , testTwo   :: Maybe Bool
                                       , testThree :: Maybe a
                                       } deriving (Eq, Show)

data family Approx a
newtype instance Approx a = Approx { fromApprox :: a }
    deriving (Show, ApproxEq, Num)

instance (ApproxEq a) => Eq (Approx a) where
    Approx a == Approx b = a =~ b

data family GADT a
data instance GADT a where
    GADT :: { gadt :: String } -> GADT String

deriving instance Eq   (GADT a)
deriving instance Show (GADT a)
