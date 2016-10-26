{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Bytes
import BasePrelude
import System.IO

_main :: Bytes.ByteString -> IO ()
_main bytes =
  case Aeson.eitherDecodeStrict' bytes of
    Right (value :: Aeson.Value) ->
      print value
    Left e -> do
      hPutStrLn stderr ("error: " <> show e)
      exitWith (ExitFailure 1)

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [file] -> do
      contents <- Bytes.readFile file
      _main contents
    _ -> do
      hPutStrLn stderr ("Usage: " <> progName <> " file.json")
      exitWith (ExitFailure 1)
