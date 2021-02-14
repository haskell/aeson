{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Data.Aeson
import           Prelude.Compat

import           Control.DeepSeq      (NFData)
import           Criterion.Main       (Benchmark, bench, bgroup, defaultMain,
                                       env, nf)
import           Data.Maybe           (fromMaybe)
import           Data.Proxy           (Proxy (..))
import           Data.Vector          (Vector)
import           System.Environment   (lookupEnv)
import           System.FilePath      ((</>))

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Twitter
import qualified Twitter.Manual       ()

import qualified GitHub

-------------------------------------------------------------------------------
-- Decode bench
-------------------------------------------------------------------------------

decodeBench
  :: forall a. (FromJSON a, NFData a)
  => String    -- ^ name
  -> FilePath  -- ^ input file
  -> Proxy a   -- ^ what type
  -> Benchmark
decodeBench name fp _ = bgroup name
    [ env (readL fp) $ \contents -> bench "lazy"   $ nf decL contents
    , env (readS fp) $ \contents -> bench "strict" $ nf decS contents
    ]
  where
    decL :: LBS.ByteString -> Maybe a
    decL = decode

    decS :: BS.ByteString -> Maybe a
    decS = decodeStrict

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

readS :: FilePath -> IO BS.ByteString
readS fp = do
    dataDir <- lookupEnv "AESON_BENCH_DATADIR"
    BS.readFile $ maybe id (</>) dataDir fp

readL :: FilePath -> IO LBS.ByteString
readL fp = do
    dataDir <- lookupEnv "AESON_BENCH_DATADIR"
    LBS.readFile $ fromMaybe "json-data" dataDir </> fp

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "Examples"
    [ bgroup "decode"
      [ decodeBench "twitter100"    "twitter100.json"    (Proxy :: Proxy Twitter.Result)
      , decodeBench "jp100"         "jp100.json"         (Proxy :: Proxy Twitter.Result)
      , decodeBench "github-issues" "github-issues.json" (Proxy :: Proxy (Vector GitHub.Issue))
      ]
    ]
  ]
