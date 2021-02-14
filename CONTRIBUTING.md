# Documenting changes

Changes that affect users of aeson should be mentioned in `changelog.md`. It should contain
* A description
* The level of change, see [PVP](https://wiki.haskell.org/Package_versioning_policy)
* Where applicable
  * How to migrate existing code
  * When it should be used
  * What it supersedes

Add this entry under an `## Upcoming` header above existing releases.

The exact format of entries is not important as we'll go through everything before a release is made.

# Style guide

* 4 space indentation

## Pragmas

* One pragma on each line
* No alignment
* Sort lexicographically

```haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
```

## Module declaration and exports

* Opening paren on its own line
* Prefix with commas
* End with `) where`

```haskell
module Data.Aeson.Types.ToJSON
    (
    -- * Core JSON classes
      ToJSON(..)
    -- * Liftings to unary and binary type constructors
    , ToJSON1(..)
    , toJSON1
    ) where
```

## Imports

* No alignment of import lists or qualifications
* Sort lexicographically (qualified imports will be last)
* No space between types and constructor lists
* Sort identifiers in import lists lexicographically

```haskell
import BarBar as B ((<>), foo)
import Foo as F
import Qux (Q(..), q)
import qualified A
```

## Development Workflow (If you plan on using Stack)

Here we outline a general workflow new contributors could adopt for a nice incremental development experience.

The root folder consists of two stack-*.yaml config files.

1. stack-nightly.yaml - Config file defines all the settings to build and test any changes.
2. stack-bench.yaml - Used to run benchmark tests.
3. stack-ffi-unescape.yaml - This is for replacing certain parts with a C-implementation.

Therefore, a ghci development experience would be:

1. `stack ghci --stack-yaml stack-nightly.yaml --test`
2. :r to recompile
3. `main` to run all tests, or `:main --pattern Foo` to run specific tests matched by pattern.

Some discussion on this topic can also be found here [here](https://github.com/bos/aeson/issues/733)

Of course before submitting a PR, the following steps are recommended:

1. 'stack test --stack-yaml stack-nightly.yaml` - Run the entire suite of tests
2. `make hlint` - Run hlint on the source folders.
3. 'stack test --stack-yaml stack-bench.yaml` - Run the benchmark tests if you believe your changes could affect the benchmarks.

## Development Workflow (If you plan on using Cabal)

A ghci development experience would be:

1. `cabal new-repl aeson-tests`
2. `:r` to recompile
3. `Main.main`  to run all tests, or `:m Main; :main --pattern Foo` to run specific tests matched by pattern.

Of course before submitting a PR, the following steps are recommended:

1. `cabal new-test` - Run the entire suite of tests
2. `make hlint` - Run hlint on the source folders.
3. Uncomment the `benchmarks` line in `cabal.project` - Run the benchmark tests if you believe your changes could affect the benchmarks.

### Running benchmarks

You need to install `cabal-plan` and `criterion-cmp`:

```
cabal install cabal-plan criterion-cmp
```

Then to build benchmarks we use a different project, which builds
`aeson` as a package with a different name to avoid rebuilding `criterion`
etc tools all the time. There is a helper script which usage
can be as simple as:

```
git checkout master
./bench.sh run -n master
git checkout your-branch
./bench.sh run -n your-branch
./bench.sh compare master your-branch
```

which will output a table like

```
Benchmark                             master   your-branch
Examples/decode/github-issues/lazy    1.77e-3  1.76e-3 -0.68%
Examples/decode/github-issues/strict  1.75e-3  1.69e-3 -3.29%
Examples/decode/jp100/lazy            1.97e-3  1.98e-3 +0.43%
Examples/decode/jp100/strict          1.94e-3  1.96e-3 +1.10%
Examples/decode/twitter100/lazy       1.54e-3  1.59e-3 +2.98%
Examples/decode/twitter100/strict     1.51e-3  1.51e-3 -0.20%
```

Run `./bench.sh help` for more details.
