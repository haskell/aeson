{-# LANGUAGE NoImplicitPrelude #-}

module Typed (benchmark) where

import           Bench
import qualified Typed.Generic  as Generic
import qualified Typed.Manual   as Manual
import qualified Typed.TH       as TH

benchmark :: Benchmark
benchmark = bgroup "Twitter"
  [ bgroup "encode"
    [ Generic.benchmarks
    , Manual.benchmarks
    , TH.benchmarks
    ]
  , bgroup "decode"
    [ Generic.decodeBenchmarks
    , Manual.decodeBenchmarks
    , TH.decodeBenchmarks
    ]
  ]
