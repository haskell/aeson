{-# LANGUAGE CPP #-}
module Bench (
    Benchmark,
    bench,
    bgroup,
    nf,
    whnf,
    env,
    defaultMain,
    Benchmarkable,
) where

#ifdef MIN_VERSION_criterion
import Criterion.Main
#else
import Test.Tasty.Bench
#endif
