{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module PropertyRoundTrip ( roundTripTests ) where

import Prelude.Compat

import Control.Applicative (Const)
import Data.Aeson.Types
import Data.DList (DList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Proxy (Proxy)
import Data.Ratio (Ratio)
import Data.Sequence (Seq)
import Data.Tagged (Tagged)
import Data.These (These (..))
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear)
import Data.Version (Version)
import Data.Time.Calendar.Compat (CalendarDiffDays, DayOfWeek)
import Data.Time.LocalTime.Compat (CalendarDiffTime)
import Data.Time.Clock.System.Compat (SystemTime)
import Data.Tuple.Solo (Solo)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Short as ST
import qualified Data.UUID.Types as UUID
import qualified Data.Strict as S
import qualified Data.Fix as F
import PropUtils
import PropertyRTFunctors
import Data.Int (Int8)

import Instances ()

roundTripTests :: TestTree
roundTripTests =
  testGroup "roundTrip" [
      testProperty "Value" $ roundTripEq @Value
    , testProperty "Bool" $ roundTripEq @Bool
    , testProperty "Double" $ roundTripEq @(Approx Double)
    , testProperty "Int" $ roundTripEq @Int
    , testProperty "NonEmpty Char" $ roundTripEq @(NonEmpty Char)
    , testProperty "Integer" $ roundTripEq @Integer
    , testProperty "String" $ roundTripEq @String
    , testProperty "Text" $ roundTripEq @T.Text
    , testProperty "Lazy Text" $ roundTripEq @LT.Text
    , testProperty "Foo" $ roundTripEq @Foo
    , testProperty "Day" $ roundTripEq @Day
    , testProperty "Month" $ roundTripEq @Month
    , testProperty "Quarter" $ roundTripEq @Quarter
    , testProperty "QuarterOfYear" $ roundTripEq @QuarterOfYear
    , testProperty "BCE Day" $ roundTripEq @BCEDay
    , testProperty "DotNetTime" $ roundTripEq @(Approx DotNetTime)
    , testProperty "LocalTime" $ roundTripEq @LocalTime
    , testProperty "TimeOfDay" $ roundTripEq @TimeOfDay
    , testProperty "UTCTime" $ roundTripEq @UTCTime
    , testProperty "ZonedTime" $ roundTripEq @ZonedTime
    , testProperty "NominalDiffTime" $ roundTripEq @NominalDiffTime
    , testProperty "DiffTime" $ roundTripEq @DiffTime
    , testProperty "DayOfWeek" $ roundTripEq @DayOfWeek
    , testProperty "SystemTime" $ roundTripEq @SystemTime
    , testProperty "CalendarDiffTime" $ roundTripEq @CalendarDiffTime
    , testProperty "CalendarDiffDays" $ roundTripEq @CalendarDiffDays
    , testProperty "Version" $ roundTripEq @Version
    , testProperty "Natural" $ roundTripEq @Natural
    , testProperty "Proxy" $ roundTripEq @(Proxy Int)
    , testProperty "Tagged" $ roundTripEq @(Tagged Int Char)
    , testProperty "Const" $ roundTripEq @(Const Int Char)
    , testProperty "DList" $ roundTripEq @(DList Int)
    , testProperty "Seq" $ roundTripEq @(Seq Int)
    , testProperty "Rational" $ roundTripEq @Rational
    , testProperty "Ratio Int" $ roundTripEq @(Ratio Int)
    , testProperty "UUID" $ roundTripEq @UUID.UUID
    , testProperty "These" $ roundTripEq @(These Char Bool)
    , testProperty "Fix" $ roundTripEq @(F.Fix (These Char))
    , testProperty "Mu" $ roundTripEq @(F.Mu (These Char))
    , testProperty "Nu" $ roundTripEq @(F.Nu (These Char))
    , testProperty "Strict Pair" $ roundTripEq @(S.Pair Int Char)
    , testProperty "Strict Either" $ roundTripEq @(S.Either Int Char)
    , testProperty "Strict These" $ roundTripEq @(S.These Int Char)
    , testProperty "Strict Maybe" $ roundTripEq @(S.Maybe Int)
    , testProperty "Solo Int" $ roundTripEq @(Solo Int)
    , testProperty "ShortText" $ roundTripEq @(ST.ShortText)
    , roundTripFunctorsTests
    , testGroup "ghcGenerics" [
        testProperty "OneConstructor" $ roundTripEq OneConstructor
      , testProperty "Product2" $ roundTripEq @(Product2 Int Bool)
      , testProperty "Product6" $ roundTripEq @(Product6 Int Bool String (Approx Double) (Int, Approx Double) ())
      , testProperty "Sum4" $ roundTripEq @(Sum4 Int8 ZonedTime T.Text (Map String Int))
      ]
    ]
