{-# LANGUAGE NoImplicitPrelude #-}

module PropertyKeys ( keysTests ) where

import Prelude.Compat

import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
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
    [ testProperty "Bool" $ roundTripKey True
    , testProperty "Text" $ roundTripKey (undefined :: T.Text)
    , testProperty "String" $ roundTripKey (undefined :: String)
    , testProperty "Int" $ roundTripKey (undefined :: Int)
    , testProperty "[Text]" $ roundTripKey (undefined :: LogScaled [T.Text])
    , testProperty "(Int,Char)" $ roundTripKey (undefined :: (Int,Char))
    , testProperty "Integer" $ roundTripKey (undefined :: Integer)
    , testProperty "Natural" $ roundTripKey (undefined :: Natural)
    , testProperty "Float" $ roundTripKey (undefined :: Float)
    , testProperty "Double" $ roundTripKey (undefined :: Double)
    , testProperty "Day" $ roundTripKey (undefined :: Day)
    , testProperty "LocalTime" $ roundTripKey (undefined :: LocalTime)
    , testProperty "TimeOfDay" $ roundTripKey (undefined :: TimeOfDay)
    , testProperty "UTCTime" $ roundTripKey (undefined :: UTCTime)
    , testProperty "Version" $ roundTripKey (undefined :: Version)
    , testProperty "Lazy Text" $ roundTripKey (undefined :: LT.Text)
    , testProperty "UUID" $ roundTripKey UUID.nil
    ]
