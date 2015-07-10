-- Manually write instances.

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Twitter.Manual
    (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where

import Control.Applicative
import Data.Aeson hiding (Result)
import Data.Monoid ((<>))
import Prelude hiding (id)
import Twitter

instance ToJSON Metadata where
  toJSON Metadata{..} = object [
      "result_type" .= result_type
    ]

  toEncoding Metadata{..} = pairs $
    "result_type" .= result_type

instance FromJSON Metadata where
  parseJSON (Object v) = Metadata <$> v .: "result_type"
  parseJSON _          = empty

instance ToJSON Geo where
  toJSON Geo{..} = object [
      "type_"       .= type_
    , "coordinates" .= coordinates
    ]

  toEncoding Geo{..} = pairs $
       "type_"       .= type_
    <> "coordinates" .= coordinates

instance FromJSON Geo where
  parseJSON (Object v) = Geo <$>
        v .: "type_"
    <*> v .: "coordinates"
  parseJSON _          = empty

instance ToJSON Story where
  toJSON Story{..} = object [
      "from_user_id_str"  .= from_user_id_str
    , "profile_image_url" .= profile_image_url
    , "created_at"        .= created_at
    , "from_user"         .= from_user
    , "id_str"            .= id_str
    , "metadata"          .= metadata
    , "to_user_id"        .= to_user_id
    , "text"              .= text
    , "id"                .= id
    , "from_user_id"      .= from_user_id
    , "geo"               .= geo
    , "iso_language_code" .= iso_language_code
    , "to_user_id_str"    .= to_user_id_str
    , "source"            .= source
    ]

  toEncoding Story{..} = pairs $
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

instance FromJSON Story where
  parseJSON (Object v) = Story <$>
        v .: "from_user_id_str"
    <*> v .: "profile_image_url"
    <*> v .: "created_at"
    <*> v .: "from_user"
    <*> v .: "id_str"
    <*> v .: "metadata"
    <*> v .: "to_user_id"
    <*> v .: "text"
    <*> v .: "id"
    <*> v .: "from_user_id"
    <*> v .: "geo"
    <*> v .: "iso_language_code"
    <*> v .: "to_user_id_str"
    <*> v .: "source"
  parseJSON _ = empty

instance ToJSON Result where
  toJSON Result{..} = object [
      "results"          .= results
    , "max_id"           .= max_id
    , "since_id"         .= since_id
    , "refresh_url"      .= refresh_url
    , "next_page"        .= next_page
    , "results_per_page" .= results_per_page
    , "page"             .= page
    , "completed_in"     .= completed_in
    , "since_id_str"     .= since_id_str
    , "max_id_str"       .= max_id_str
    , "query"            .= query
    ]

  toEncoding Result{..} = pairs $
       "results"          .= results
    <> "max_id"           .= max_id
    <> "since_id"         .= since_id
    <> "refresh_url"      .= refresh_url
    <> "next_page"        .= next_page
    <> "results_per_page" .= results_per_page
    <> "page"             .= page
    <> "completed_in"     .= completed_in
    <> "since_id_str"     .= since_id_str
    <> "max_id_str"       .= max_id_str
    <> "query"            .= query

instance FromJSON Result where
  parseJSON (Object v) = Result <$>
        v .: "results"
    <*> v .: "max_id"
    <*> v .: "since_id"
    <*> v .: "refresh_url"
    <*> v .: "next_page"
    <*> v .: "results_per_page"
    <*> v .: "page"
    <*> v .: "completed_in"
    <*> v .: "since_id_str"
    <*> v .: "max_id_str"
    <*> v .: "query"
  parseJSON _ = empty
