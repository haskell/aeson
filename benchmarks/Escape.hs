{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Criterion.Main
import qualified "aeson-benchmarks" Data.Aeson.Parser.UnescapeFFI as FFI
import qualified "aeson-benchmarks" Data.Aeson.Parser.UnescapePure as Pure

import qualified Data.ByteString.Char8 as BS

n :: Int
n = 10000

input :: BS.ByteString
input = BS.concat $ replicate n $ BS.pack "\\\""

main :: IO ()
main = defaultMain
    [ bench "ffi"  $ whnf FFI.unescapeText input
    , bench "pure" $ whnf Pure.unescapeText input
    ]
