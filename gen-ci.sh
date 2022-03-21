#!/bin/sh

# haskell-ci regenerate doesn't regenerate bench project CI
haskell-ci github cabal.project
haskell-ci github -o .github/workflows/z-bench.yml --config=cabal.bench.haskell-ci --project cabal.bench.project
