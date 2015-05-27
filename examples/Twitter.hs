-- These types follow the format of Twitter search results, as can be
-- found in the benchmarks/json-data directory.
--
-- For uses of these types, see the Twitter subdirectory.
--
-- There is one deviation for the sake of convenience: the Geo field
-- named "type_" is really named "type" in Twitter's real feed.  I
-- renamed "type" to "type_" in the *.json files, to avoid overlap
-- with a Haskell reserved keyword.

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Twitter (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where

import Data.Data (Typeable, Data)
import Data.Int (Int64)
import Data.Text (Text)
-- import Data.Time (ZonedTime)
import GHC.Generics (Generic)
import Prelude hiding (id)
import Control.DeepSeq

data Metadata = Metadata {
    result_type :: Text
  } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData Metadata

data Geo = Geo {
    type_       :: Text
  , coordinates :: (Double, Double)
  } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData Geo

data Story = Story {
    from_user_id_str  :: Text
  , profile_image_url :: Text
  , created_at        :: Text -- ZonedTime
  , from_user         :: Text
  , id_str            :: Text
  , metadata          :: Metadata
  , to_user_id        :: Maybe Int64
  , text              :: Text
  , id                :: Int64
  , from_user_id      :: Int64
  , geo               :: Maybe Geo
  , iso_language_code :: Text
  , to_user_id_str    :: Maybe Text
  , source            :: Text
  } deriving (Show, Typeable, Data, Generic)

instance NFData Story

data Result = Result {
    results          :: [Story]
  , max_id           :: Int64
  , since_id         :: Int64
  , refresh_url      :: Text
  , next_page        :: Text
  , results_per_page :: Int
  , page             :: Int
  , completed_in     :: Double
  , since_id_str     :: Text
  , max_id_str       :: Text
  , query            :: Text
  } deriving (Show, Typeable, Data, Generic)

instance NFData Result
