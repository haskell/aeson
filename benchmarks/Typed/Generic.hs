module Typed.Generic (benchmarks, decodeBenchmarks) where

import Prelude ()
import Prelude.Compat

import Data.Aeson hiding (Result)
import Criterion
import Data.ByteString.Lazy as L
import Twitter.Generic
import Typed.Common

encodeDirect :: Result -> L.ByteString
encodeDirect = encode

encodeViaValue :: Result -> L.ByteString
encodeViaValue = encode . toJSON

benchmarks :: Benchmark
benchmarks =
  env ((,) <$> load "json-data/twitter100.json" <*> load "json-data/jp100.json") $ \ ~(twitter100, jp100) ->
  bgroup "encodeGeneric" [
      bgroup "direct" [
        bench "twitter100" $ nf encodeDirect twitter100
      , bench "jp100"      $ nf encodeDirect jp100
      ]
    , bgroup "viaValue" [
        bench "twitter100" $ nf encodeViaValue twitter100
      , bench "jp100"      $ nf encodeViaValue jp100
      ]
    ]

decodeDirect :: L.ByteString -> Maybe Result
decodeDirect = decode

decodeBenchmarks :: Benchmark
decodeBenchmarks =
  env ((,) <$> L.readFile "json-data/twitter100.json" <*> L.readFile "json-data/jp100.json") $ \ ~(twitter100, jp100) ->
  bgroup "decodeGeneric"
    [ bgroup "direct"
      [ bench "twitter100" $ nf decodeDirect twitter100
      , bench "jp100"      $ nf decodeDirect jp100
      ]
    ]
