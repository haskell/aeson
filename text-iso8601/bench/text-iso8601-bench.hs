{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import Test.Tasty.Bench (defaultMain, bench, nf)

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Time as A

import Data.Time.FromText (parseUTCTime)

main :: IO ()
main = defaultMain
    [ bench "text" $ nf parseUTCTime input1
    , bench "atto" $ nf (runAtto A.utcTime) input1
    ]

input1 :: Text
input1 = "2023-06-09T16:53:55Z"
{-# NOINLINE input1 #-}

runAtto :: A.Parser a -> Text -> Either String a
runAtto p t = A.parseOnly (p <* A.endOfInput) t
