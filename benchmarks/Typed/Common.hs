{-# LANGUAGE CPP, PackageImports #-}
module Typed.Common (load) where

#ifndef HAS_BOTH_AESON_AND_BENCHMARKS
import Data.Aeson hiding (Result)
#else
import "aeson" Data.Aeson hiding (Result)
import qualified "aeson-benchmarks" Data.Aeson as B
#endif

import Data.ByteString.Lazy as L
import Control.Applicative
import System.IO
import System.Exit

load :: FromJSON a => FilePath -> IO a
load fileName = do
  mv <- eitherDecode' <$> L.readFile fileName
  case mv of
    Right v -> return v
    Left err -> do
      hPutStrLn stderr $ fileName ++ ": JSON decode failed - " ++ err
      exitWith (ExitFailure 1)
