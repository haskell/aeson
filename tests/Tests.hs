module Main (main) where

import Test.Framework (defaultMain)
import qualified Properties
import qualified UnitTests

main :: IO ()
main = do
    ioTests <- UnitTests.ioTests
    defaultMain (Properties.tests : UnitTests.tests : ioTests)
