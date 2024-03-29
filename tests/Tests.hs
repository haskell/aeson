{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Prelude.Compat

import Test.Tasty (defaultMain, testGroup)
import qualified DataFamilies.Properties as DF
import qualified Properties
import qualified UnitTests
import qualified JSONTestSuite
import RFC8785

main :: IO ()
main = do
    ioTests <- UnitTests.ioTests
    jsTests <- JSONTestSuite.tests
    let allTests = DF.tests : rfc8785Tests : Properties.tests : UnitTests.tests : jsTests : ioTests
    defaultMain (testGroup "tests" allTests)
