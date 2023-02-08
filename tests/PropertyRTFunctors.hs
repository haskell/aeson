{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module PropertyRTFunctors ( roundTripFunctorsTests ) where

import Prelude.Compat

import Data.Functor.Compose (Compose (..))
import Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types
import PropUtils


roundTripFunctorsTests :: TestTree
roundTripFunctorsTests =
  testGroup "functors"
      [ testProperty "Identity Char" $ roundTripEq @(I Int)

      , testProperty "Identity Char" $ roundTripEq @(I Char)
      , testProperty "Identity [Char]" $ roundTripEq @(I String)
      , testProperty "[Identity Char]" $ roundTripEq @([I Char])

      , testProperty "Compose I  I  Int" $ roundTripEq @(LogScaled (Compose I  I  Int))
      , testProperty "Compose [] I  Int" $ roundTripEq @(LogScaled (Compose [] I  Int))
      , testProperty "Compose I  [] Int" $ roundTripEq @(LogScaled (Compose I  [] Int))
      , testProperty "Compose [] [] Int" $ roundTripEq @(LogScaled (Compose [] [] Int))

      , testProperty "Compose I  I  Char" $ roundTripEq @(LogScaled (Compose I  I  Char))
      , testProperty "Compose [] I  Char" $ roundTripEq @(LogScaled (Compose [] I  Char))
      , testProperty "Compose I  [] Char" $ roundTripEq @(LogScaled (Compose I  [] Char))
      , testProperty "Compose [] [] Char" $ roundTripEq @(LogScaled (Compose [] [] Char))

      , testProperty "Compose3 I  I  I  Char" $ roundTripEq @(LogScaled (Compose3 I  I  I  Char))
      , testProperty "Compose3 I  [] I  Char" $ roundTripEq @(LogScaled (Compose3 I  [] I  Char))
      , testProperty "Compose3 I  I  [] Char" $ roundTripEq @(LogScaled (Compose3 I  I  [] Char))
      , testProperty "Compose3 I  [] [] Char" $ roundTripEq @(LogScaled (Compose3 I  [] [] Char))
      , testProperty "Compose3 [] I  I  Char" $ roundTripEq @(LogScaled (Compose3 [] I  I  Char))
      , testProperty "Compose3 [] [] I  Char" $ roundTripEq @(LogScaled (Compose3 [] [] I  Char))
      , testProperty "Compose3 [] I  [] Char" $ roundTripEq @(LogScaled (Compose3 [] I  [] Char))
      , testProperty "Compose3 [] [] [] Char" $ roundTripEq @(LogScaled (Compose3 [] [] [] Char))

      , testProperty "Compose3' I  I  I  Char" $ roundTripEq @(LogScaled (Compose3' I  I  I  Char))
      , testProperty "Compose3' I  [] I  Char" $ roundTripEq @(LogScaled (Compose3' I  [] I  Char))
      , testProperty "Compose3' I  I  [] Char" $ roundTripEq @(LogScaled (Compose3' I  I  [] Char))
      , testProperty "Compose3' I  [] [] Char" $ roundTripEq @(LogScaled (Compose3' I  [] [] Char))
      , testProperty "Compose3' [] I  I  Char" $ roundTripEq @(LogScaled (Compose3' [] I  I  Char))
      , testProperty "Compose3' [] [] I  Char" $ roundTripEq @(LogScaled (Compose3' [] [] I  Char))
      , testProperty "Compose3' [] I  [] Char" $ roundTripEq @(LogScaled (Compose3' [] I  [] Char))
      , testProperty "Compose3' [] [] [] Char" $ roundTripEq @(LogScaled (Compose3' [] [] [] Char))
      ]
