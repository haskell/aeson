{-# LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DataFamilies.Instances where

import Control.Applicative
import Data.Aeson.TH
import DataFamilies.Types
import Test.QuickCheck (Arbitrary(..), elements, oneof)
import Prelude

instance (Arbitrary a) => Arbitrary (Approx a) where
    arbitrary = Approx <$> arbitrary

instance Arbitrary (Nullary Int) where
    arbitrary = elements [C1, C2, C3]

instance Arbitrary a => Arbitrary (SomeType c () a) where
    arbitrary = oneof [ pure Nullary
                      , Unary   <$> arbitrary
                      , Product <$> arbitrary <*> arbitrary <*> arbitrary
                      , Record  <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

deriveJSON defaultOptions 'C1
deriveJSON defaultOptions 'Nullary
deriveJSON defaultOptions 'Approx
