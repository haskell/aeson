-- Manually write instances.

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Twitter.Manual
    (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative
import Data.Monoid ((<>))
import Prelude hiding (id)
import Twitter

#ifndef HAS_BOTH_AESON_AND_BENCHMARKS
import Data.Aeson hiding (Result)
#else
import "aeson" Data.Aeson hiding (Result)
import qualified "aeson-benchmarks" Data.Aeson as B
#endif

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
    , "id"                .= id_
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
    <> "id"                .= id_
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

#ifdef HAS_BOTH_AESON_AND_BENCHMARKS
instance B.ToJSON Metadata where
  toJSON Metadata{..} = B.object [
      "result_type" B..= result_type
    ]

  toEncoding Metadata{..} = B.pairs $
    "result_type" B..= result_type

instance B.FromJSON Metadata where
  parseJSON (B.Object v) = Metadata <$> v B..: "result_type"
  parseJSON _          = empty

instance B.ToJSON Geo where
  toJSON Geo{..} = B.object [
      "type_"       B..= type_
    , "coordinates" B..= coordinates
    ]

  toEncoding Geo{..} = B.pairs $
       "type_"       B..= type_
    <> "coordinates" B..= coordinates

instance B.FromJSON Geo where
  parseJSON (B.Object v) = Geo <$>
        v B..: "type_"
    <*> v B..: "coordinates"
  parseJSON _          = empty

instance B.ToJSON Story where
  toJSON Story{..} = B.object [
      "from_user_id_str"  B..= from_user_id_str
    , "profile_image_url" B..= profile_image_url
    , "created_at"        B..= created_at
    , "from_user"         B..= from_user
    , "id_str"            B..= id_str
    , "metadata"          B..= metadata
    , "to_user_id"        B..= to_user_id
    , "text"              B..= text
    , "id"                B..= id_
    , "from_user_id"      B..= from_user_id
    , "geo"               B..= geo
    , "iso_language_code" B..= iso_language_code
    , "to_user_id_str"    B..= to_user_id_str
    , "source"            B..= source
    ]

  toEncoding Story{..} = B.pairs $
       "from_user_id_str"  B..= from_user_id_str
    <> "profile_image_url" B..= profile_image_url
    <> "created_at"        B..= created_at
    <> "from_user"         B..= from_user
    <> "id_str"            B..= id_str
    <> "metadata"          B..= metadata
    <> "to_user_id"        B..= to_user_id
    <> "text"              B..= text
    <> "id"                B..= id_
    <> "from_user_id"      B..= from_user_id
    <> "geo"               B..= geo
    <> "iso_language_code" B..= iso_language_code
    <> "to_user_id_str"    B..= to_user_id_str
    <> "source"            B..= source

instance B.FromJSON Story where
  parseJSON (B.Object v) = Story <$>
        v B..: "from_user_id_str"
    <*> v B..: "profile_image_url"
    <*> v B..: "created_at"
    <*> v B..: "from_user"
    <*> v B..: "id_str"
    <*> v B..: "metadata"
    <*> v B..: "to_user_id"
    <*> v B..: "text"
    <*> v B..: "id"
    <*> v B..: "from_user_id"
    <*> v B..: "geo"
    <*> v B..: "iso_language_code"
    <*> v B..: "to_user_id_str"
    <*> v B..: "source"
  parseJSON _ = empty

instance B.ToJSON Result where
  toJSON Result{..} = B.object [
      "results"          B..= results
    , "max_id"           B..= max_id
    , "since_id"         B..= since_id
    , "refresh_url"      B..= refresh_url
    , "next_page"        B..= next_page
    , "results_per_page" B..= results_per_page
    , "page"             B..= page
    , "completed_in"     B..= completed_in
    , "since_id_str"     B..= since_id_str
    , "max_id_str"       B..= max_id_str
    , "query"            B..= query
    ]

  toEncoding Result{..} = B.pairs $
       "results"          B..= results
    <> "max_id"           B..= max_id
    <> "since_id"         B..= since_id
    <> "refresh_url"      B..= refresh_url
    <> "next_page"        B..= next_page
    <> "results_per_page" B..= results_per_page
    <> "page"             B..= page
    <> "completed_in"     B..= completed_in
    <> "since_id_str"     B..= since_id_str
    <> "max_id_str"       B..= max_id_str
    <> "query"            B..= query

instance B.FromJSON Result where
  parseJSON (B.Object v) = Result <$>
        v B..: "results"
    <*> v B..: "max_id"
    <*> v B..: "since_id"
    <*> v B..: "refresh_url"
    <*> v B..: "next_page"
    <*> v B..: "results_per_page"
    <*> v B..: "page"
    <*> v B..: "completed_in"
    <*> v B..: "since_id_str"
    <*> v B..: "max_id_str"
    <*> v B..: "query"
  parseJSON _ = empty
#endif
