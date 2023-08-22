{-# LANGUAGE NoImplicitPrelude #-}

module Dates (benchmark) where

import Prelude.Compat
import Bench

import Data.Aeson (decode, encode)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime)
import qualified Data.ByteString.Lazy as BL

import Utils (readL)

utcTime :: BL.ByteString -> Maybe [UTCTime]
utcTime = decode

zTime :: BL.ByteString -> Maybe [ZonedTime]
zTime = decode

benchmark :: Benchmark
benchmark =
  bgroup "dates" [
      bgroup "decode" [
        bgroup "UTCTime" [
          env file1 $ \bs -> bench "whole" $ nf utcTime bs
        , env file2 $ \bs -> bench "fractional" $ nf utcTime bs
        ]
      , bgroup "ZonedTime" [
          env file1 $ \bs -> bench "whole" $ nf zTime bs
        , env file2 $ \bs -> bench "fractional" $ nf zTime bs
        ]
      ]
    , bgroup "encode" [
        bgroup "UTCTime" [
          env (utcTime <$> file1) $ \ts -> bench "whole" $ nf encode ts
        , env (utcTime <$> file2) $ \ts -> bench "fractional" $ nf encode ts
        ]
      , bgroup "ZonedTime" [
          env (zTime <$> file1) $ \ts -> bench "whole" $ nf encode ts
        , env (zTime <$> file2) $ \ts -> bench "fractional" $ nf encode ts
        ]
      ]
    ]
  where
    file1 = readL "dates.json"
    file2 = readL "dates-fract.json"
