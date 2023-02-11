{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Data.Aeson
import           Prelude.Compat

import           Bench                      (Benchmark, bench, bgroup,
                                             defaultMain, env, nf, whnf)
import           Control.DeepSeq            (NFData)
import           Data.Aeson.Parser.Internal (unescapeText)
import           Data.Proxy                 (Proxy (..))
import           Data.Vector                (Vector)
import qualified Data.Aeson.Decoding        as Dec

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE

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
import qualified UnescapePureText2          as Text2

import           Utils

#if !MIN_VERSION_text(2,0,0)
import qualified UnescapePureText1          as Text1
#endif

#if __GLASGOW_HASKELL__ >=810
import qualified CompareWithHermes
#endif

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
    [ env (readL fp) $ \contents -> bench "lazy"       $ nf decL contents
    , env (readS fp) $ \contents -> bench "strict"     $ nf decS contents
    , env (readL fp) $ \contents -> bench "lazy tok"   $ nf decU contents
    , env (readS fp) $ \contents -> bench "strict tok" $ nf decT contents
    ]
  where
    decL :: LBS.ByteString -> Maybe a
    decL = decode

    decS :: BS.ByteString -> Maybe a
    decS = decodeStrict

    decT :: BS.ByteString -> Maybe a
    decT = Dec.decodeStrict

    decU :: LBS.ByteString -> Maybe a
    decU = Dec.decode

-------------------------------------------------------------------------------
-- Escape bench
-------------------------------------------------------------------------------

escapeBench :: Benchmark
escapeBench = bgroup "Escape"
    [ example "ascii" $ BS8.pack $ take 500 $ cycle ['a'..'z']
    , example "cyrillic" $ TE.encodeUtf8 $ T.unwords
      [ "Стандарт состоит из двух основных частей: универсального набора "
      , "символов (англ. Universal character set, UCS) и семейства кодировок"
      , "(англ. Unicode transformation format, UTF). Универсальный набор"
      , "символов перечисляет допустимые по стандарту Юникод символы и"
      , "присваивает каждому символу код в виде неотрицательного целого"
      , "числа, записываемого обычно в шестнадцатеричной форме с префиксом"
      , "U+, например, U+040F. Семейство кодировок определяет способы"
      , "преобразования кодов символов для передачи в потоке или в файле."
      ]
    , example "hexEscapes" $
        let chars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['\x10000' .. '\x100ff']
            formatChar :: Char -> B.Builder
            formatChar c = case BS.length bs of
                4 -> "\\u" <> B.byteString bs
                8 -> "\\u" <> B.byteString (BS.take 4 bs) <> "\\u" <> B.byteString (BS.drop 4 bs)
                _ -> error "formatChar: ???"
              where
                bs = Base16.encode $ TE.encodeUtf16BE $ T.pack [c]

        in LBS.toStrict $ B.toLazyByteString $ foldMap formatChar chars
    , example "charEscapes" $
        BS.concat $ replicate 10000 $ BS8.pack "\\\""
    ]
  where
    example :: String -> BS.ByteString -> Benchmark
    example name input = bgroup name
      [ bench "def"   $ whnf unescapeText input
#if !MIN_VERSION_text(2,0,0)
      , bench "text1" $ whnf Text1.unescapeText input
#endif
      , bench "text2" $ whnf Text2.unescapeText input
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
#if __GLASGOW_HASKELL__ >=810
    ++ [ CompareWithHermes.benchmark ]
#endif
