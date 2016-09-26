module Options () where

import Prelude ()
import Prelude.Compat

import Data.Aeson.Types

opts :: Options
opts = defaultOptions
       { sumEncoding = ObjectWithSingleField
       }
