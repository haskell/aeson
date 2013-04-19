{-# Language OverloadedStrings, RecordWildCards, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Types
import Control.Monad
import Test.QuickCheck (Arbitrary(..), choose, Gen)
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
    arbitrary = picosecondsToDiffTime `liftM` choose (0, 86400000000000000)

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary

instance Arbitrary DotNetTime where
    arbitrary = DotNetTime `liftM` arbitrary

instance Arbitrary ZonedTime where
    arbitrary = liftM2 ZonedTime arbitrary arbitrary

deriving instance Eq ZonedTime

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
