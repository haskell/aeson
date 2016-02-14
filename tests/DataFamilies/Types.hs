{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module DataFamilies.Types where

import Types (ApproxEq(..))

#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics
#endif

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

-- We only derive instances for GHC 7.6 and higher because GHC 7.4 has a bug
-- concerning generics and data families

#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic (Nullary Int)
deriving instance Generic (Nullary Char)
deriving instance Generic (SomeType c () a)
deriving instance Generic (Approx a)
#endif
