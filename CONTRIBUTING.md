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

                                                                                                                                              
                                                                                                                                                                                                                    
## Development Workflow                                                                                                                                                                                             
                                                                                                                                                                                                                    
Here we outline a general workflow new contributors could adopt for a nice incremental development experience.                                                                                                      
                                                                                                                                                                                                                    
The root folder consists of two stack-*.yaml config files.                                                                                                                                                          
                                                                                                                                                                                                                    
1. stack-night.yaml - Config file defines all the settings to build and test any changes.                                                                                                                           
2. stack-bench.yaml - Used to run benchmark tests.                                                                                                                                                                  
                                                                                                                                                                                                                    
Therefore, a ghci development experience would be:                                                                                                                                                                 
                                                                                                                                                                                                                    
1. `stack ghci --stack-yaml stack-nightly.yaml --test`                                                                                                                                                              
2. :r to reload                                                                                                                                                                                                    
3. `main` to run all tests, or `:main --pattern Foo` to run specific tests matched by pattern.                                                                                                                     
                                                                                                                                                                                                                    
Some discussion on this topic can also be found here [here](https://github.com/bos/aeson/issues/733)    

Of course before submitting a PR, the following steps are recommended:

1. Run the entire suite of tests and benchmarks.
2. Run hlint on the source folders. (Note: the 4 space indentation specified at the top of this document)
                                                                 
