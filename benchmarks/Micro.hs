{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Prelude.Compat

import Criterion.Main

-- Encoding is a newtype wrapper around Builder
import Data.Aeson.Encoding (text, string, encodingToLazyByteString)
import qualified Data.Text as T

main :: IO ()
main = do
  let txt = "append (append b (primBounded w1 x1)) (primBounded w2 x2)"
  defaultMain [
    bgroup "string" [
      bench "text" $ nf (encodingToLazyByteString . text) (T.pack txt)
    , bench "string direct" $ nf (encodingToLazyByteString . string) txt
    , bench "string via text" $ nf (encodingToLazyByteString . text . T.pack) txt
    ]
   ]
