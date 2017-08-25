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
