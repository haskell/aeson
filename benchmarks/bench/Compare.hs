{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compare (benchmarks) where

import Prelude.Compat

import Compare.BufferBuilder ()
import Criterion.Main
import Data.BufferBuilder.Json
import Twitter
import Twitter.Manual ()
import Utils (readV)
import qualified Data.Aeson as Aeson
import qualified Compare.JsonBench as JsonBench

#ifdef MIN_VERSION_json_builder
import Data.Json.Builder
import Compare.JsonBuilder ()
#endif

benchmarks :: [Benchmark]
benchmarks =
  [ env (readV "twitter100.json") $ \ ~(twtr :: Result) ->
    bgroup "CompareEncodeTwitter"
    [ bench "aeson" $ nf Aeson.encode twtr
    , bench "buffer-builder" $ nf encodeJson twtr
#ifdef MIN_VERSION_json_builder
    , bench "json-builder" $ nf toJsonLBS twtr
#endif
    ]
  , JsonBench.benchmarks
  ]
