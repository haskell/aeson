{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Data.Aeson
import           Prelude.Compat

import           Control.DeepSeq                (NFData)
import           Criterion.Main                 (Benchmark, bench, bgroup,
                                                 defaultMain, env, nf, whnf)
import           Data.Maybe                     (fromMaybe)
import           Data.Proxy                     (Proxy (..))
import           Data.Vector                    (Vector)
import           System.Environment             (lookupEnv)
import           System.FilePath                ((</>))

import qualified Data.Aeson.Encoding.Builder    as Aeson.EB
import qualified Data.Aeson.Parser.UnescapeFFI  as FFI
import qualified Data.Aeson.Parser.UnescapePure as Pure
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Builder        as B
import qualified Data.ByteString.Char8          as BS8
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Text                      as T

import qualified GitHub
import qualified Issue673
import qualified Twitter
import qualified Twitter.Manual                 ()

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
-- Escape bench
-------------------------------------------------------------------------------

escapeBench :: Benchmark
escapeBench = bgroup "Escape"
    [ example "ascii" $ BS8.pack $ take 500 $ cycle ['a'..'z']
    , example "cyrillic" $ LBS.toStrict $ B.toLazyByteString $ Aeson.EB.unquoted $ T.unwords
      [ "Стандарт состоит из двух основных частей: универсального набора "
      , "символов (англ. Universal character set, UCS) и семейства кодировок"
      , "(англ. Unicode transformation format, UTF). Универсальный набор"
      , "символов перечисляет допустимые по стандарту Юникод символы и"
      , "присваивает каждому символу код в виде неотрицательного целого"
      , "числа, записываемого обычно в шестнадцатеричной форме с префиксом"
      , "U+, например, U+040F. Семейство кодировок определяет способы"
      , "преобразования кодов символов для передачи в потоке или в файле."
      ]
    ]
  where
    example :: String -> BS.ByteString -> Benchmark
    example name input = bgroup name
      [ bench "ffi"  $ whnf FFI.unescapeText input
      , bench "pure" $ whnf Pure.unescapeText input
      ]

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

readS :: FilePath -> IO BS.ByteString
readS fp = do
    dataDir <- lookupEnv "AESON_BENCH_DATADIR"
    BS.readFile $ fromMaybe "json-data" dataDir </> fp

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
  , escapeBench
  , Issue673.benchmark
  ]
