{-# LANGUAGE NoImplicitPrelude #-}

module Micro (benchmark) where

import Prelude.Compat
import Bench

-- Encoding is a newtype wrapper around Builder
import Data.Aeson.Encoding (text, string, encodingToLazyByteString)
import qualified Data.Text as T

benchmark :: Benchmark
benchmark = bgroup "micro"
  [ bgroup "string"
    [ bench "text" $ nf (encodingToLazyByteString . text) (T.pack txt)
    , bench "string direct" $ nf (encodingToLazyByteString . string) txt
    , bench "string via text" $ nf (encodingToLazyByteString . text . T.pack) txt
    ]
   ]
  where txt = "append (append b (primBounded w1 x1)) (primBounded w2 x2)"
