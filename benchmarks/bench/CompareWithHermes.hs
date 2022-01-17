{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CompareWithHermes (benchmark) where

import Prelude.Compat

import Data.Coerce
import Criterion.Main
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Parser.Internal as I
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Hermes as H
import qualified Twitter as T
import Twitter.Manual () -- fair comparison with manual Hermes decoders

import Utils

decode :: BL.ByteString -> T.Result
decode s = fromMaybe (error "fail to parse via Aeson") $ A.decode s

decode' :: BL.ByteString -> T.Result
decode' s = fromMaybe (error "fail to parse via Aeson") $ A.decode' s

decodeS :: BS.ByteString -> T.Result
decodeS s = fromMaybe (error "fail to parse via Aeson") $ A.decodeStrict' s

decodeIP :: BL.ByteString -> T.Result
decodeIP s = fromMaybe (error "fail to parse via Parser.decodeWith") $
    I.decodeWith I.jsonEOF A.fromJSON s

decodeH :: BS.ByteString -> T.Result
decodeH s = case H.decodeEither twitterResultDecoder s of
  Right result -> result
  Left err -> error (show err)

twitterResultDecoder :: H.Value -> H.Decoder T.Result
twitterResultDecoder = H.withObject $ \obj ->
  T.Result
    <$> H.atKey "results" (H.list storyDecoder) obj
    <*> H.atKey "max_id" int64 obj
    <*> H.atKey "since_id" int64 obj
    <*> H.atKey "refresh_url" H.text obj
    <*> H.atKey "next_page" H.text obj
    <*> H.atKey "results_per_page" H.int obj
    <*> H.atKey "page" H.int obj
    <*> H.atKey "completed_in" H.double obj
    <*> H.atKey "since_id_str" H.text obj
    <*> H.atKey "max_id_str" H.text obj
    <*> H.atKey "query" H.text obj

storyDecoder :: H.Value -> H.Decoder T.Story
storyDecoder = H.withObject $ \obj ->
  T.Story
    <$> H.atKey "from_user_id_str" H.text obj
    <*> H.atKey "profile_image_url" H.text obj
    <*> H.atKey "created_at" H.text obj
    <*> H.atKey "from_user" H.text obj
    <*> H.atKey "id_str" H.text obj
    <*> H.atKey "metadata" metadataDecoder obj
    <*> H.atKey "to_user_id" (H.nullable int64) obj
    <*> H.atKey "text" H.text obj
    <*> H.atKey "id" int64 obj
    <*> H.atKey "from_user_id" int64 obj 
    <*> pure Nothing -- our bench corpus doesn't have any geolocated tweets
    <*> H.atKey "iso_language_code" H.text obj
    <*> H.atKey "to_user_id_str" (H.nullable H.text) obj
    <*> H.atKey "source" H.text obj

int64 = fmap fromIntegral <$> H.int

metadataDecoder :: H.Value -> H.Decoder T.Metadata
metadataDecoder = H.withObject $ \obj ->
  T.Metadata <$> H.atKey "result_type" H.text obj

encodeToText :: A.Value -> TL.Text
encodeToText = TLB.toLazyText . A.encodeToTextBuilder . A.toJSON

encodeViaText :: A.Value -> BL.ByteString
encodeViaText = TLE.encodeUtf8 . encodeToText

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
        , bench "hermes"         $ nf decodeH enS
        ]
      , bgroup "jp" [
          bench "aeson"          $ nf decode jpA
        , bench "aeson/stricter" $ nf decodeS jpS
        , bench "hermes"         $ nf decodeH jpS
        ]
      ]
    ]
  where
    enFile = "twitter100.json"
    jpFile = "jp100.json"
