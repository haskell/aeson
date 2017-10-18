#!/bin/sh

set -ex

case $BUILD in
  stack)
    stack build --test --haddock --no-terminal
    ;;
  cabal)
    if [ -f configure.ac ]; then autoreconf -i; fi
    cabal configure --enable-tests --enable-benchmarks -v2  # -v2 provides useful information for debugging
    cabal build   # this builds all libraries and executables (including tests/benchmarks)
    cabal test
    cabal sdist   # tests that a source-distribution can be generated

    # Check that the resulting source distribution can be built & installed.
    # If there are no other `.tar.gz` files in `dist`, this can be even simpler:
    # `cabal install --force-reinstalls dist/*-*.tar.gz`
    SRC_TGZ=$(cabal info . | awk '{print $2;exit}').tar.gz &&
      (cd dist && cabal install --force-reinstalls "$SRC_TGZ")
    ;;
  hlint)
    stack build --fast aeson --stack-yaml stack-nightly.yaml --system-ghc --no-terminal
    stack install hlint --stack-yaml stack-nightly.yaml --system-ghc --no-terminal
    make lint
    ;;
esac
