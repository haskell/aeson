{-# LANGUAGE NoImplicitPrelude #-}

module Typed.TH (benchmarks, decodeBenchmarks) where

import Prelude.Compat

import Data.Aeson hiding (Result)
import Criterion
import Data.ByteString.Lazy as L
import Twitter.TH
import Utils

encodeDirect :: Result -> L.ByteString
encodeDirect = encode

encodeViaValue :: Result -> L.ByteString
encodeViaValue = encode . toJSON

benchmarks :: Benchmark
benchmarks =
  env ((,) <$> readV "twitter100.json" <*> readV "jp100.json") $ \ ~(twitter100, jp100) ->
  bgroup "TH" [
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
  env ((,) <$> readL "twitter100.json" <*> readL "jp100.json") $ \ ~(twitter100, jp100) ->
  bgroup "TH"
    [ bgroup "direct"
      [ bench "twitter100" $ nf decodeDirect twitter100
      , bench "jp100"      $ nf decodeDirect jp100
      ]
    ]
