module Main where

--------------------------------------------------------------------------------

import Criterion.Main
import Control.DeepSeq (deepseq)
import Data.Aeson

--------------------------------------------------------------------------------

type FJ a = Value -> Result a

type T2 = (Int, Int)
type T3 = (Int, Int, Int)
type T4 = (Int, Int, Int, Int)

t2 :: T2
t2 = (1, 2)

t3 :: T3
t3 = (1, 2, 3)

t4 :: T4
t4 = (1, 2, 3, 4)

main :: IO ()
main = let v2 = toJSON t2
           v3 = toJSON t3
           v4 = toJSON t4
       in t2 `deepseq` t3 `deepseq` t4 `deepseq`
          v2 `deepseq` v3 `deepseq` v4 `deepseq`
            defaultMain
              [ bgroup "t2"
                [ bench "toJSON"   (nf toJSON t2)
                , bench "fromJSON" (nf (fromJSON :: FJ T2) v2)
                ]
              , bgroup "t3"
                [ bench "toJSON"   (nf toJSON t3)
                , bench "fromJSON" (nf (fromJSON :: FJ T3) v3)
                ]
              , bgroup "t4"
                [ bench "toJSON"   (nf toJSON t4)
                , bench "fromJSON" (nf (fromJSON :: FJ T4) v4)
                ]
              ]
