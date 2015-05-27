module Main (main) where

import Criterion.Main
import qualified Typed.Generic as Generic
import qualified Typed.Manual as Manual

main :: IO ()
main = defaultMain [
    Generic.benchmarks
  , Manual.benchmarks
  ]
