-- Use Template Haskell to generate good instances.

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Twitter.TH
    (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where

import Prelude ()

import Twitter

#ifndef HAS_BOTH_AESON_AND_BENCHMARKS
import Data.Aeson.TH
#else
import "aeson" Data.Aeson.TH
import qualified "aeson-benchmarks" Data.Aeson.TH as B
#endif

$(deriveJSON defaultOptions ''Metadata)
$(deriveJSON defaultOptions ''Geo)
$(deriveJSON defaultOptions ''Story)
$(deriveJSON defaultOptions ''Result)

#ifdef HAS_BOTH_AESON_AND_BENCHMARKS
$(B.deriveJSON B.defaultOptions ''Metadata)
$(B.deriveJSON B.defaultOptions ''Geo)
$(B.deriveJSON B.defaultOptions ''Story)
$(B.deriveJSON B.defaultOptions ''Result)
#endif
