{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Typed.Common (load) where

import Prelude ()
import Prelude.Compat

import Data.ByteString.Lazy as L
import System.Exit
import System.IO

#ifndef HAS_BOTH_AESON_AND_BENCHMARKS
import Data.Aeson hiding (Result)
#else
import "aeson" Data.Aeson hiding (Result)
import qualified "aeson-benchmarks" Data.Aeson as B
#endif

load :: FromJSON a => FilePath -> IO a
load fileName = do
  mv <- eitherDecode' <$> L.readFile fileName
  case mv of
    Right v -> return v
    Left err -> do
      hPutStrLn stderr $ fileName ++ ": JSON decode failed - " ++ err
      exitWith (ExitFailure 1)
