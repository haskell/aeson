{-# LANGUAGE DeriveGeneric #-}

module Auto.G.BigRecord where

import Control.DeepSeq
import Data.Aeson
import GHC.Generics (Generic)
import Options

data BigRecord = BigRecord
    { field01 :: !Int, field02 :: !Int, field03 :: !Int, field04 :: !Int, field05 :: !Int
    , field06 :: !Int, field07 :: !Int, field08 :: !Int, field09 :: !Int, field10 :: !Int
    , field11 :: !Int, field12 :: !Int, field13 :: !Int, field14 :: !Int, field15 :: !Int
    , field16 :: !Int, field17 :: !Int, field18 :: !Int, field19 :: !Int, field20 :: !Int
    , field21 :: !Int, field22 :: !Int, field23 :: !Int, field24 :: !Int, field25 :: !Int
    } deriving (Show, Eq, Generic)

instance NFData BigRecord where
  rnf a = a `seq` ()

instance ToJSON BigRecord where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

instance FromJSON BigRecord where
  parseJSON = genericParseJSON opts

bigRecord :: BigRecord
bigRecord = BigRecord 1   2  3  4  5
                      6   7  8  9 10
                      11 12 13 14 15
                      16 17 18 19 20
                      21 22 23 24 25
