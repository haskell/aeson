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
