{-# LANGUAGE TemplateHaskell #-}

module Auto.T.BigSum where

import Control.DeepSeq
import Data.Aeson.TH
import Options

data BigSum = F01 | F02 | F03 | F04 | F05
            | F06 | F07 | F08 | F09 | F10
            | F11 | F12 | F13 | F14 | F15
            | F16 | F17 | F18 | F19 | F20
            | F21 | F22 | F23 | F24 | F25
    deriving (Show, Eq)

instance NFData BigSum where
  rnf a = a `seq` ()

deriveJSON opts ''BigSum

bigSum :: BigSum
bigSum = F25
