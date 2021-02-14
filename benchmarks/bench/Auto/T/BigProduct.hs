{-# LANGUAGE TemplateHaskell #-}

module Auto.T.BigProduct where

import Control.DeepSeq
import Data.Aeson.TH
import Options

data BigProduct = BigProduct
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    deriving (Show, Eq)

instance NFData BigProduct where
  rnf a = a `seq` ()

deriveJSON opts ''BigProduct

bigProduct :: BigProduct
bigProduct = BigProduct 1   2  3  4  5
                        6   7  8  9 10
                        11 12 13 14 15
                        16 17 18 19 20
                        21 22 23 24 25
