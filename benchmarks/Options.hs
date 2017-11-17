module Options (opts) where

import Data.Aeson.Types

opts :: Options
opts = defaultOptions
       { sumEncoding = ObjectWithSingleField
       }
