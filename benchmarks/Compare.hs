{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Compare.BufferBuilder ()
import Compare.JsonBuilder ()
import Criterion.Main
import Data.BufferBuilder.Json
import Data.Json.Builder
import Twitter
import Twitter.Manual ()
import Typed.Common
import qualified Compare.JsonBench as JsonBench
import qualified Data.Aeson as Aeson

main :: IO ()
main =
  defaultMain [
     env (load "json-data/twitter100.json") $ \ ~(twtr :: Result) ->
     bgroup "twitter" [
         bench "aeson" $ nf Aeson.encode twtr
       , bench "buffer-builder" $ nf encodeJson twtr
       , bench "json-builder" $ nf toJsonLBS twtr
       ]
   , JsonBench.benchmarks
   ]
