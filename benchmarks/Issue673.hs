{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main,
    input17,
    input32,
    input64,
    input128,
    input256,
    input2048,
    input4096,
    input8192,
    input16384,
  ) where

import Criterion.Main
import Prelude.Compat
import Data.Int (Int64)
import Data.Scientific (Scientific)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8

decodeInt :: LBS.ByteString -> Maybe Int
decodeInt = A.decode

decodeScientific :: LBS.ByteString -> Maybe Scientific
decodeScientific = A.decode

generate :: Int64 -> LBS.ByteString
generate n = LBS8.replicate n '1'

input17 :: LBS.ByteString
input17 = generate 17

input32 :: LBS.ByteString
input32 = generate 32

input64 :: LBS.ByteString
input64 = generate 64

input128 :: LBS.ByteString
input128 = generate 128

input256 :: LBS.ByteString
input256 = generate 256

input2048 :: LBS.ByteString
input2048 = generate 2048

input4096 :: LBS.ByteString
input4096 = generate 4096

input8192 :: LBS.ByteString
input8192 = generate 8192

input16384 :: LBS.ByteString
input16384 = generate 16384


main :: IO ()
main =  defaultMain
    -- works on 64bit
    [ benchPair "17" input17
    -- , benchPair "32" input32
    -- , benchPair "64" input64
    -- , benchPair "128" input128
    -- , benchPair "256" input256
    , benchPair "2048" input2048
    , benchPair "4096" input4096
    , benchPair "8192" input8192
    , benchPair "16384" input16384
    ]
  where
    benchPair name input = bgroup name
        [ bench "Int" $ whnf decodeInt input
        , bench "Scientific" $ whnf decodeScientific input
        ]
