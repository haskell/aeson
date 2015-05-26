module Main (main) where

import qualified Data.ByteString.Lazy as L
import Criterion.Main
import Data.Aeson
import Typed.Manual
import Control.Applicative ((<$>))
import System.Exit
import System.IO

deeecode :: FromJSON a => FilePath -> IO a
deeecode fileName = do
  mv <- eitherDecode' <$> L.readFile fileName
  case mv of
    Right v -> return v
    Left err -> do
      hPutStrLn stderr $ fileName ++ ": JSON decode failed - " ++ err
      exitWith (ExitFailure 1)

main :: IO ()
main = do
  twitter100 <- deeecode "json-data/twitter100.json"
  jp100 <- deeecode "json-data/jp100.json"
  defaultMain [
      bgroup "direct" [
        bench "twitter100" $ nf encodeDirect twitter100
      , bench "jp100" $ nf encodeDirect jp100
      ]
    , bgroup "viaValue" [
        bench "twitter100" $ nf encodeViaValue twitter100
      , bench "jp100" $ nf encodeViaValue jp100
      ]
    ]
