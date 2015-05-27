module Typed.Common (load) where

import Data.Aeson hiding (Result)
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
