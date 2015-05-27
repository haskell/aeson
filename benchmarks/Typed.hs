module Main (main) where

import Criterion.Main
import qualified Typed.Generic as Generic
import qualified Typed.Manual as Manual
import qualified Typed.TH as TH

main :: IO ()
main = defaultMain [
    Generic.benchmarks
  , Manual.benchmarks
  , TH.benchmarks
  ]
