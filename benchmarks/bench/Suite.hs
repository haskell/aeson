{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Data.Aeson
import           Prelude.Compat

import           Control.DeepSeq                (NFData)
import           Criterion.Main                 (Benchmark, bench, bgroup,
                                                 defaultMain, env, nf, whnf)
import           Data.Proxy                     (Proxy (..))
import           Data.Vector                    (Vector)

import qualified Data.Aeson.Encoding.Builder    as Aeson.EB
import qualified Data.Aeson.Parser.UnescapeFFI  as FFI
import qualified Data.Aeson.Parser.UnescapePure as Pure
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Builder        as B
import qualified Data.ByteString.Char8          as BS8
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Text                      as T

import qualified AesonFoldable
import qualified AesonMap
import qualified AutoCompare
import qualified Compare
import qualified CompareWithJSON
import qualified Dates
import qualified GitHub
import qualified Issue673
import qualified Micro
import qualified Typed

import           Utils

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
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  AutoCompare.sanityCheck
  defaultMain $
    [ bgroup "Examples"
      [ bgroup "decode"
        [ decodeBench "github-issues" "github-issues.json" (Proxy :: Proxy (Vector GitHub.Issue))
        ]
      ]
    , escapeBench              -- text decoding
    , Issue673.benchmark       -- issue673: big integers
    , Typed.benchmark          -- Twitter
    , AutoCompare.benchmark    -- compares Generic and TH
    , Micro.benchmark          -- benchmarking some tight loops
    , Dates.benchmark          -- dates
    , AesonMap.benchmark       -- maps
    , AesonFoldable.benchmark  -- different ways to encode foldable containers
    ]
    ++ Compare.benchmarks -- compares to different libs (encoding)
    ++ [ CompareWithJSON.benchmark ]
