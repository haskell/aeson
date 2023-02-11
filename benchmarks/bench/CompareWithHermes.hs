{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CompareWithHermes (benchmark) where

import Prelude.Compat
import Bench

import Data.Maybe (fromMaybe)
import qualified Data.Aeson as A
import qualified Data.Aeson.Decoding as A.D
import qualified Data.Aeson.Parser.Internal as I
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Hermes as H
import qualified Data.Hermes.Aeson as H.A
import qualified Twitter as T
import Twitter.Manual () -- fair comparison with manual Hermes decoders
import Twitter.Hermes

import Utils

decode :: BL.ByteString -> T.Result
decode s = fromMaybe (error "fail to parse via Aeson") $ A.decode s

decode' :: BL.ByteString -> T.Result
decode' s = fromMaybe (error "fail to parse via Aeson") $ A.decode' s

decodeS :: BS.ByteString -> T.Result
decodeS s = fromMaybe (error "fail to parse via Aeson") $ A.decodeStrict' s

decodeTS :: BS.ByteString -> T.Result
decodeTS s = fromMaybe (error "fail to parse via Aeson") $ A.D.decodeStrict s

decodeIP :: BL.ByteString -> T.Result
decodeIP s = fromMaybe (error "fail to parse via Parser.decodeWith") $
    I.decodeWith I.jsonEOF A.fromJSON s

decodeH :: BS.ByteString -> T.Result
decodeH s = case H.decodeEither twitterResultDecoder s of
  Right result -> result
  Left err -> error (show err)

-- decode to A.Value, and then use A.FromJSON instance.
decodeHA :: BS.ByteString -> T.Result
decodeHA s = case H.decodeEither H.A.valueDecoder s of
  Left err -> error (show err)
  Right v  -> case A.fromJSON v of
      A.Success x -> x
      A.Error err -> error (show err)

benchmark :: Benchmark
benchmark =
  env (readL enFile) $ \enA ->
  env (readS enFile) $ \enS ->
  env (readL jpFile) $ \jpA ->
  env (readS jpFile) $ \jpS ->
  bgroup "compare-hermes" [
      bgroup "decode" [
        bgroup "en" [
          bench "aeson/lazy"     $ nf decode enA
        , bench "aeson/strict"   $ nf decode' enA
        , bench "aeson/stricter" $ nf decodeS enS
        , bench "aeson/parser"   $ nf decodeIP enA
        , bench "aeson/tokens/strict" $ nf decodeTS enS
        , bench "hermes"         $ nf decodeH enS
        , bench "hermes/aeson"   $ nf decodeHA enS
        ]
      , bgroup "jp" [
          bench "aeson"          $ nf decode jpA
        , bench "aeson/stricter" $ nf decodeS jpS
        , bench "aeson/tokens/strict" $ nf decodeTS jpS
        , bench "hermes"         $ nf decodeH jpS
        , bench "hermes/aeson"   $ nf decodeHA jpS
        ]
      ]
    ]
  where
    enFile = "twitter100.json"
    jpFile = "jp100.json"
