module Main (main) where

import Test.Framework (defaultMain)
import qualified DataFamilies.Properties as DF
import qualified Properties
import qualified UnitTests

main :: IO ()
main = do
    ioTests <- UnitTests.ioTests
    defaultMain (DF.tests : Properties.tests : UnitTests.tests : ioTests)
