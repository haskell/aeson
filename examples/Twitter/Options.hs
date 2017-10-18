{-# LANGUAGE CPP #-}

#ifdef HAS_BOTH_AESON_AND_BENCHMARKS
{-# LANGUAGE PackageImports #-}
#endif

module Twitter.Options (module Twitter.Options) where

#ifndef HAS_BOTH_AESON_AND_BENCHMARKS
import Data.Aeson
import Data.Aeson.Types
#else
import "aeson" Data.Aeson
import qualified "aeson-benchmarks" Data.Aeson as B
#endif

twitterOptions :: Options
twitterOptions = defaultOptions
    { fieldLabelModifier = \x -> case x of
        "id_" -> "id"
        _     -> x
    }

#ifdef HAS_BOTH_AESON_AND_BENCHMARKS
btwitterOptions :: B.Options
btwitterOptions = B.defaultOptions
    { B.fieldLabelModifier = \x -> case x of
        "id_" -> "id"
        _     -> x
    }
#endif
