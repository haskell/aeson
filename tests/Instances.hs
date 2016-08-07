{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Types
import Data.Function (on)
import Control.Monad
import Test.QuickCheck (Arbitrary(..), getNonNegative, elements,
                        listOf1, oneof, resize)
import Data.Time.Clock (UTCTime(..))
import Data.Time (ZonedTime(..), TimeZone(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Version
import Data.Aeson.Types
import Control.Applicative (Const(..))
import Data.Tagged (Tagged(..))
import Data.Proxy (Proxy(..))
import Control.Applicative
import Functions

import Data.Orphans ()
import Test.QuickCheck.Instances ()

#if !MIN_VERSION_base(4,8,0) && !MIN_VERSION_QuickCheck(2,8,3)
import Numeric.Natural
#endif

-- "System" types.

instance Arbitrary DotNetTime where
    arbitrary = DotNetTime `liftM` arbitrary
    shrink = map DotNetTime . shrink . fromDotNetTime

-- | Compare timezone part only on 'timeZoneMinutes'
instance Eq ZonedTime where
  ZonedTime a (TimeZone a' _ _) == ZonedTime b (TimeZone b' _ _) =
    a == b && a' == b'

-- Compare equality to within a millisecond, allowing for rounding
-- error (ECMA 262 requires milliseconds to rounded to zero, not
-- rounded to nearest).
instance ApproxEq UTCTime where
    a =~ b = ((==) `on` utctDay) a b &&
             (approxEqWith 1 1 `on` ((* 1e3) . utctDayTime)) a b

instance ApproxEq DotNetTime where
    (=~) = (=~) `on` fromDotNetTime

instance ApproxEq Double where
    (=~) = approxEq

-- Test-related types.

instance Arbitrary Foo where
    arbitrary = liftM4 Foo arbitrary arbitrary arbitrary arbitrary

instance Eq Foo where
    a == b = fooInt a == fooInt b &&
             fooDouble a `approxEq` fooDouble b &&
             fooTuple a == fooTuple b

instance ToJSON Foo where
    toJSON Foo{..} = object [ "fooInt" .= fooInt
                            , "fooDouble" .= fooDouble
                            , "fooTuple" .= fooTuple
                            , "fooMap" .= fooMap
                            ]

instance FromJSON Foo where
    parseJSON (Object v) = Foo <$>
                           v .: "fooInt" <*>
                           v .: "fooDouble" <*>
                           v .: "fooTuple" <*>
                           v .: "fooMap"
    parseJSON _ = empty

instance Arbitrary UFoo where
    arbitrary = UFoo <$> arbitrary <*> arbitrary
        where _ = uFooInt

instance Arbitrary OneConstructor where
    arbitrary = return OneConstructor

instance FromJSON OneConstructor
instance ToJSON OneConstructor

instance (Arbitrary a, Arbitrary b) => Arbitrary (Product2 a b) where
    arbitrary = liftM2 Product2 arbitrary arbitrary

instance (FromJSON a, FromJSON b) => FromJSON (Product2 a b)
instance (ToJSON a, ToJSON b) => ToJSON (Product2 a b)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e,
          Arbitrary f) => Arbitrary (Product6 a b c d e f) where
    arbitrary = Product6 <$> arbitrary <*> arbitrary <*> arbitrary <*>
                             arbitrary <*> arbitrary <*> arbitrary

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f) => FromJSON (Product6 a b c d e f)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e,
          ToJSON f) => ToJSON (Product6 a b c d e f)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Sum4 a b c d) where
    arbitrary = oneof [Alt1 <$> arbitrary, Alt2 <$> arbitrary,
                       Alt3 <$> arbitrary, Alt4 <$> arbitrary]

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d)
    => FromJSON (Sum4 a b c d)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (Sum4 a b c d)

instance (Arbitrary a) => Arbitrary (Approx a) where
    arbitrary = Approx <$> arbitrary

instance (FromJSON a) => FromJSON (Approx a) where
    parseJSON a = Approx <$> parseJSON a

instance (ToJSON a) => ToJSON (Approx a) where
    toJSON = toJSON . fromApprox

instance Arbitrary Nullary where
    arbitrary = elements [C1, C2, C3]

instance Arbitrary a => Arbitrary (SomeType a) where
    arbitrary = oneof [ pure Nullary
                      , Unary   <$> arbitrary
                      , Product <$> arbitrary <*> arbitrary <*> arbitrary
                      , Record  <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

instance Arbitrary (GADT String) where
    arbitrary = GADT <$> arbitrary

instance ApproxEq Char where
    (=~) = (==)

instance (ApproxEq a) => ApproxEq [a] where
    a =~ b = length a == length b && all (uncurry (=~)) (zip a b)

#if !MIN_VERSION_QuickCheck(2,9,0)
instance Arbitrary Version where
    arbitrary = makeVersion . fmap getNonNegative <$> resize 4 (listOf1 arbitrary)

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Const a b) where
    arbitrary = Const <$> arbitrary
#endif

-- Version tags are deprecated, so we avoid using them in the Arbitrary
-- instance. However, the recommended constructor 'makeVersion' is not
-- exported by "Data.Version" until base-4.8.0.0. For previous versions,
-- a definition is given below.

#if !MIN_VERSION_base(4,8,0)
makeVersion :: [Int] -> Version
makeVersion b = Version b []
#endif

#if !MIN_VERSION_base(4,8,0) && !MIN_VERSION_QuickCheck(2,8,3)
instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary
#endif

instance Arbitrary (Proxy a) where
    arbitrary = pure Proxy

instance Arbitrary b => Arbitrary (Tagged a b) where
    arbitrary = Tagged <$> arbitrary
