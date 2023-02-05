{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compare (benchmarks) where

import Prelude.Compat
import Bench

import Twitter
import Twitter.Manual ()
import Utils (readV)
import qualified Data.Aeson as Aeson
import qualified Compare.JsonBench as JsonBench

#ifdef MIN_VERSION_buffer_builder
import Compare.BufferBuilder ()
import Data.BufferBuilder.Json
#endif

#ifdef MIN_VERSION_json_builder
import Data.Json.Builder
import Compare.JsonBuilder ()
#endif

benchmarks :: [Benchmark]
benchmarks =
  [ env (readV "twitter100.json") $ \ ~(twtr :: Result) ->
    bgroup "CompareEncodeTwitter"
    [ bench "aeson" $ nf Aeson.encode twtr
#ifdef MIN_VERSION_buffer_builder
    , bench "buffer-builder" $ nf encodeJson twtr
#endif
#ifdef MIN_VERSION_json_builder
    , bench "json-builder" $ nf toJsonLBS twtr
#endif
    ]
  , JsonBench.benchmarks
  ]
