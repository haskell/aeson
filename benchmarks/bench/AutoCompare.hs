{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AutoCompare (benchmark, sanityCheck) where

import Control.DeepSeq
import Control.Monad
import Bench
import Data.Aeson

import qualified Auto.T.D as T
import qualified Auto.T.BigRecord as T
import qualified Auto.T.BigProduct as T
import qualified Auto.T.BigSum as T
import qualified Auto.G.D as G
import qualified Auto.G.BigRecord as G
import qualified Auto.G.BigProduct as G
import qualified Auto.G.BigSum as G

--------------------------------------------------------------------------------

benchmark :: Benchmark
benchmark = bgroup "AutoBench"
  [ compareBench "D" T.d G.d
  , compareBench "BigRecord" T.bigRecord G.bigRecord
  , compareBench "BigProduct" T.bigProduct G.bigProduct
  , compareBench "BigSum" T.bigSum G.bigSum
  ]

group :: String -> Benchmarkable -> Benchmarkable -> Benchmark
group n th gen = bgroup n [ bench "th"      th
                          , bench "generic" gen
                          ]

compareBench
  :: forall a b
  .  (ToJSON a, FromJSON a, NFData a, ToJSON b, FromJSON b, NFData b)
  => String -> a -> b -> Benchmark
compareBench name a b = v `deepseq` bgroup name
  [ group "toJSON"   (nf toJSON a)
                     (nf toJSON b)
  , group "encode"   (nf encode a)
                     (nf encode b)
  , group "fromJSON" (nf (fromJSON :: Value -> Result a) v)
                     (nf (fromJSON :: Value -> Result b) v)
  ] where
    v = toJSON a  -- == toJSON b

sanityCheck :: IO ()
sanityCheck = do
  check T.d
  check G.d
  check T.bigRecord
  check G.bigRecord
  check T.bigProduct
  check G.bigProduct
  check T.bigSum
  check G.bigSum

check :: (Show a, Eq a, FromJSON a, ToJSON a)
      => a -> IO ()
check x = do
  unless (Success x == (fromJSON . toJSON) x) $ fail $ "toJSON: " ++ show x
  unless (Success x == (decode_ . encode) x) $ fail $ "encode: " ++ show x
  where
    decode_ s = case decode s of
      Just v -> fromJSON v
      Nothing -> fail ""
