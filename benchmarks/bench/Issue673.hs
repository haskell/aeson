{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Issue673 (benchmark) where

import Bench
import Prelude.Compat
import Data.Int (Int64)

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8

decodeInt :: LBS.ByteString -> Maybe Int
decodeInt = A.decode

-- decodeString :: LBS.ByteString -> Maybe String
-- decodeString = A.decode

-- decodeScientific :: LBS.ByteString -> Maybe Scientific
-- decodeScientific = A.decode

-- decodeViaRead :: LBS.ByteString -> Integer
-- decodeViaRead = read . LBS8.unpack

generate :: Int64 -> LBS.ByteString
generate n = LBS8.replicate n '1'

input17 :: LBS.ByteString
input17 = generate 17

-- input32 :: LBS.ByteString
-- input32 = generate 32

-- input64 :: LBS.ByteString
-- input64 = generate 64

-- input128 :: LBS.ByteString
-- input128 = generate 128

-- input256 :: LBS.ByteString
-- input256 = generate 256

input2048 :: LBS.ByteString
input2048 = generate 2048

-- input4096 :: LBS.ByteString
-- input4096 = generate 4096

-- input8192 :: LBS.ByteString
-- input8192 = generate 8192

input16384 :: LBS.ByteString
input16384 = generate 16384

benchmark :: Benchmark
benchmark = bgroup "Integer-decoder"
    -- works on 64bit
    [ benchPair "17" input17
    -- , benchPair "32" input32
    -- , benchPair "64" input64
    -- , benchPair "128" input128
    -- , benchPair "256" input256
    , benchPair "2048" input2048
    -- , benchPair "4096" input4096
    -- , benchPair "8192" input8192
    , benchPair "16384" input16384
    ]
  where
    benchPair name input = bgroup name
        [ bench "Int"        $ whnf decodeInt input
        , bench "Simple"     $ whnf bsToIntegerSimple (LBS.toStrict input)

        -- other disabled, they are interesting for comparison only.
        -- , bench "Optim"      $ whnf bsToInteger (LBS.toStrict input)
        -- , bench "Read"       $ whnf decodeViaRead input
        -- , bench "Scientific" $ whnf decodeScientific input
        -- , bench "parserA"    $ whnf decodeAtto  input
        -- , bench "parserS"    $ whnf decodeAtto8  input
        -- , bench "String"     $ whnf decodeString $ "\"" <> input <> "\""
        ]

-------------------------------------------------------------------------------
-- better fromInteger
-------------------------------------------------------------------------------

bsToIntegerSimple :: BS.ByteString -> Integer
bsToIntegerSimple = BS.foldl' step 0 where
  step a b = a * 10 + fromIntegral (b - 48) -- 48 = '0'
