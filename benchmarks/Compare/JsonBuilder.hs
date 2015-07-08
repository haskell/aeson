{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compare.JsonBuilder () where

import Data.Json.Builder
import Data.Monoid ((<>))
import Prelude hiding (id)
import Twitter

instance JsObject Metadata where
    toObject Metadata{..} = row "result_type" result_type

instance Value Metadata where
    toJson = toJson . toObject

instance JsObject Geo where
    toObject Geo{..} =
       row "type_" type_ <>
       row "coordinates" coordinates

instance Value Geo where
    toJson = toJson . toObject

instance Value a => Value (Maybe a) where
    toJson (Just a) = toJson a
    toJson Nothing  = jsNull

instance JsObject Story where
  toObject Story{..} =
    row "from_user_id_str" from_user_id_str <>
    row "profile_image_url" profile_image_url <>
    row "created_at" created_at <>
    row "from_user" from_user <>
    row "id_str" id_str <>
    row "metadata" metadata <>
    row "to_user_id" to_user_id <>
    row "text" text <>
    row "id" id <>
    row "from_user_id" from_user_id <>
    row "geo" geo <>
    row "iso_language_code" iso_language_code <>
    row "to_user_id_str" to_user_id_str <>
    row "source" source

instance Value Story where
    toJson = toJson . toObject

instance JsObject Result where
    toObject Result{..} =
      row "results" results <>
      row "max_id" max_id <>
      row "since_id" since_id <>
      row "refresh_url" refresh_url <>
      row "next_page" next_page <>
      row "results_per_page" results_per_page <>
      row "page" page <>
      row "completed_in" completed_in <>
      row "since_id_str" since_id_str <>
      row "max_id_str" max_id_str <>
      row "query" query

instance Value Result where
    toJson = toJson . toObject
