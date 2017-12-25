module Main (main) where

import Prelude ()
import Prelude.Compat

import Test.Framework (defaultMain)
import qualified DataFamilies.Properties as DF
import qualified Properties
import qualified UnitTests
import qualified UnitTests.AccErrors as AccErrors

main :: IO ()
main = do
    ioTests <- UnitTests.ioTests
    defaultMain
        ( AccErrors.tests
        : DF.tests
        : Properties.tests
        : UnitTests.tests
        : ioTests
        )
