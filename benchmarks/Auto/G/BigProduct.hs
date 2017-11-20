{-# LANGUAGE DeriveGeneric #-}

module Auto.G.BigProduct where

import Control.DeepSeq
import Data.Aeson
import GHC.Generics (Generic)
import Options

data BigProduct = BigProduct
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    deriving (Show, Eq, Generic)

instance NFData BigProduct where
  rnf a = a `seq` ()

instance ToJSON BigProduct where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

instance FromJSON BigProduct where
  parseJSON = genericParseJSON opts

bigProduct :: BigProduct
bigProduct = BigProduct 1   2  3  4  5
                        6   7  8  9 10
                        11 12 13 14 15
                        16 17 18 19 20
                        21 22 23 24 25
