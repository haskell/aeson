module Main (main) where

import Criterion.Main
import qualified Data.Aeson.Encode.Builder as AB
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T


main :: IO ()
main = do
  let txt = "append (append b (primBounded w1 x1)) (primBounded w2 x2)"
  defaultMain [
    bgroup "string" [
      bench "text" $ nf (B.toLazyByteString . AB.text) (T.pack txt)
    , bench "string direct" $ nf (B.toLazyByteString . AB.string) txt
    , bench "string via text" $ nf (B.toLazyByteString . AB.text . T.pack) txt
    ]
   ]
