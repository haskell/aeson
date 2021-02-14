{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main (main) where

import Prelude.Compat

import Criterion.Main
import qualified Data.Aeson.Parser.UnescapeFFI as FFI
import qualified Data.Aeson.Parser.UnescapePure as Pure

import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  args_ <- getArgs
  let (args, p, n) =
        case args_ of
          "--pattern" : p : args_ -> k p args_
          _ -> k "\\\"" args_
      k p args_ =
        case args_ of
          "--repeat" : n : args_ -> (args_, p, read n)
          args_ -> (args_, p, 10000)
      input = BS.concat $ replicate n $ BS.pack p
  withArgs args $ defaultMain
    [ bench "ffi"  $ whnf FFI.unescapeText input
    , bench "pure" $ whnf Pure.unescapeText input
    ]
