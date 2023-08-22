{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Twitter.Hermes where

import Prelude.Compat

import Data.Int (Int64)

import qualified Data.Hermes as H
import qualified Twitter as T

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

int64 :: H.Value -> H.Decoder Int64
int64 = fmap fromIntegral <$> H.int

metadataDecoder :: H.Value -> H.Decoder T.Metadata
metadataDecoder = H.withObject $ \obj ->
  T.Metadata <$> H.atKey "result_type" H.text obj
