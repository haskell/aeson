{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module PropertyKeys ( keysTests ) where

import Prelude.Compat

import Control.Applicative (Const)
import Data.Time.Compat (Day, LocalTime, TimeOfDay, UTCTime)
import Data.Time.Calendar.Compat (DayOfWeek)
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear)
import Data.Version (Version)
import Instances ()
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.UUID.Types as UUID
import PropUtils


keysTests :: TestTree
keysTests =
  testGroup "roundTrip Key"
    [ testProperty "Bool"          $ roundTripKey @Bool
    , testProperty "Text"          $ roundTripKey @T.Text
    , testProperty "String"        $ roundTripKey @String
    , testProperty "Int"           $ roundTripKey @Int
    , testProperty "[Text]"        $ roundTripKey @(LogScaled [T.Text])
    , testProperty "(Int,Char)"    $ roundTripKey @(Int,Char)
    , testProperty "Integer"       $ roundTripKey @Integer
    , testProperty "Natural"       $ roundTripKey @Natural
    , testProperty "Float"         $ roundTripKey @Float
    , testProperty "Double"        $ roundTripKey @Double
    , testProperty "Day"           $ roundTripKey @Day
    , testProperty "DayOfWeek"     $ roundTripKey @DayOfWeek
    , testProperty "Month"         $ roundTripKey @Month
    , testProperty "Quarter"       $ roundTripKey @Quarter
    , testProperty "QuarterOfYear" $ roundTripKey @QuarterOfYear
    , testProperty "LocalTime"     $ roundTripKey @LocalTime
    , testProperty "TimeOfDay"     $ roundTripKey @TimeOfDay
    , testProperty "UTCTime"       $ roundTripKey @UTCTime
    , testProperty "Version"       $ roundTripKey @Version
    , testProperty "Lazy Text"     $ roundTripKey @LT.Text
    , testProperty "UUID"          $ roundTripKey @UUID.UUID
    , testProperty "Const Text"    $ roundTripKey @(Const T.Text ())
    ]
