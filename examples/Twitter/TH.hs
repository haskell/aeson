-- Use Template Haskell to generate good instances.

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifdef HAS_BOTH_AESON_AND_BENCHMARKS
{-# LANGUAGE PackageImports #-}
#endif

module Twitter.TH
    (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where

import Prelude ()

import Twitter
import Twitter.Options

#ifndef HAS_BOTH_AESON_AND_BENCHMARKS
import Data.Aeson.TH
#else
import "aeson" Data.Aeson.TH
import qualified "aeson-benchmarks" Data.Aeson.TH as B
#endif

$(deriveJSON twitterOptions ''Metadata)
$(deriveJSON twitterOptions ''Geo)
$(deriveJSON twitterOptions ''Story)
$(deriveJSON twitterOptions ''Result)

#ifdef HAS_BOTH_AESON_AND_BENCHMARKS
$(B.deriveJSON btwitterOptions ''Metadata)
$(B.deriveJSON btwitterOptions ''Geo)
$(B.deriveJSON btwitterOptions ''Story)
$(B.deriveJSON btwitterOptions ''Result)
#endif
