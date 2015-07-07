module Main (main) where

import Criterion.Main
import Data.Json.Builder
import Compare.JsonBuilder ()
import Twitter
import Twitter.Manual ()
import Typed.Common
import qualified Data.Aeson as Aeson

main :: IO ()
main = do
  twtr <- load "json-data/twitter100.json" :: IO Result
  defaultMain [
     bench "aeson" $ nf Aeson.encode twtr
   , bench "json-builder" $ nf toJsonLBS twtr
   ]
