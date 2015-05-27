-- Use Template Haskell to generate good instances.

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Twitter.TH
    (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where

import Data.Aeson.TH
import Twitter

$(deriveJSON defaultOptions ''Metadata)
$(deriveJSON defaultOptions ''Geo)
$(deriveJSON defaultOptions ''Story)
$(deriveJSON defaultOptions ''Result)
