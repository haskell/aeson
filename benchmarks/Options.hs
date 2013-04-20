module Options where

import Data.Aeson.Types

opts :: Options
opts = defaultOptions
       { sumEncoding = ObjectWithSingleField
       }
