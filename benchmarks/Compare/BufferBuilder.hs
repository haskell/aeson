{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compare.BufferBuilder () where

import Data.BufferBuilder.Json
import Data.Monoid ((<>))
import Data.Int (Int64)
import Prelude hiding (id)
import Twitter
import qualified Data.BufferBuilder.Utf8 as UB

instance (ToJson a, ToJson b) => ToJson (a,b) where
  toJson (a,b) = array [toJson a, toJson b]

instance ToJson Int64 where
    toJson a = unsafeValueUtf8Builder $
               UB.appendDecimalSignedInt (fromIntegral a)
    {-# INLINE toJson #-}

instance ToJson Metadata where
  toJson Metadata{..} = toJson $
    "result_type" .= result_type

instance ToJson Geo where
  toJson Geo{..} = toJson $
       "type_"       .= type_
    <> "coordinates" .= coordinates

instance ToJson Story where
  toJson Story{..} = toJson $
       "from_user_id_str"  .= from_user_id_str
    <> "profile_image_url" .= profile_image_url
    <> "created_at"        .= created_at
    <> "from_user"         .= from_user
    <> "id_str"            .= id_str
    <> "metadata"          .= metadata
    <> "to_user_id"        .= to_user_id
    <> "text"              .= text
    <> "id"                .= id
    <> "from_user_id"      .= from_user_id
    <> "geo"               .= geo
    <> "iso_language_code" .= iso_language_code
    <> "to_user_id_str"    .= to_user_id_str
    <> "source"            .= source

instance ToJson Result where
  toJson Result{..} = toJson $
       "results" .= results
    <> "max_id" .= max_id
    <> "since_id" .= since_id
    <> "refresh_url" .= refresh_url
    <> "next_page" .= next_page
    <> "results_per_page" .= results_per_page
    <> "page" .= page
    <> "completed_in" .= completed_in
    <> "since_id_str" .= since_id_str
    <> "max_id_str" .= max_id_str
    <> "query" .= query
