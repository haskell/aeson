module Main (main) where

import Test.Framework (defaultMain)
import qualified Properties

main :: IO ()
main = do
    comparisonTests <- Properties.encoderComparisonTests
    defaultMain (comparisonTests : Properties.tests)
