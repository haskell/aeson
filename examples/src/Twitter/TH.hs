-- Use Template Haskell to generate good instances.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Twitter.TH
    (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where


import Twitter
import Twitter.Options

import Data.Aeson.TH

$(deriveJSON twitterOptions ''Metadata)
$(deriveJSON twitterOptions ''Geo)
$(deriveJSON twitterOptions ''Story)
$(deriveJSON twitterOptions ''Result)
