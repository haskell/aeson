{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Prelude.Compat

import Compare.BufferBuilder ()
import Criterion.Main
import Data.BufferBuilder.Json
import Twitter
import Twitter.Manual ()
import Typed.Common
import qualified Data.Aeson as Aeson
import qualified Compare.JsonBench as JsonBench

#ifdef MIN_VERSION_json_builder
import Data.Json.Builder
import Compare.JsonBuilder ()
#endif

main :: IO ()
main =
  defaultMain [
     env (load "json-data/twitter100.json") $ \ ~(twtr :: Result) ->
     bgroup "twitter" [
         bench "aeson" $ nf Aeson.encode twtr
       , bench "buffer-builder" $ nf encodeJson twtr
#ifdef MIN_VERSION_json_builder
       , bench "json-builder" $ nf toJsonLBS twtr
#endif
       ]
   , JsonBench.benchmarks
   ]
