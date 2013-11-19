{-# Language CPP, OverloadedStrings, RecordWildCards, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ < 702
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
#endif

module Instances where

import Types
import Data.Function (on)
import Control.Monad
import Test.QuickCheck (Arbitrary(..), Gen, choose, oneof, elements)
import Data.Time.Clock (DiffTime, UTCTime(..), picosecondsToDiffTime)
import Data.Time (ZonedTime(..), LocalTime(..), TimeZone(..),
                  hoursToTimeZone, Day(..), TimeOfDay(..))
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Aeson.Types
import Control.Applicative
import Functions

-- "System" types.

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> arbitrary

instance Arbitrary LocalTime where
    arbitrary = return $ LocalTime (ModifiedJulianDay 1) (TimeOfDay 1 2 3)

instance Arbitrary TimeZone where
    arbitrary = do
      offset <- choose (0,2) :: Gen Int
      return $ hoursToTimeZone offset

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay `liftM` arbitrary

instance Arbitrary DiffTime where
    arbitrary = (picosecondsToDiffTime . (* 1000000000)) <$>
                choose (0, 86400000)

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary

instance Arbitrary DotNetTime where
    arbitrary = DotNetTime `liftM` arbitrary

instance Arbitrary ZonedTime where
    arbitrary = liftM2 ZonedTime arbitrary arbitrary

deriving instance Eq ZonedTime

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
