# 0.3 to 0.4

## Generics support

Aeson's support for data-type generic programming makes it possible to
use JSON encodings of most data types without writing any boilerplate
instances.

Thanks to Bas Van Dijk, aeson now supports the two major schemes for
doing datatype-generic programming:

* the newer mechanism, [built into GHC
  itself](http://www.haskell.org/ghc/docs/latest/html/users_guide/generic-programming.html)

* the older mechanism, based on SYB (for "scrap your boilerplate")

The GHC-based generics are fast and terse: in fact, they're generally
comparable in performance to hand-written `ToJSON` and `FromJSON`
instances.  To see how to use GHC generics, see
[`examples/Generic.hs`](https://github.com/bos/aeson/blob/master/examples/Generic.hs).

The SYB-based generics support lives in
[Data.Aeson.Generic](http://hackage.haskell.org/packages/archive/aeson/0.4.0.0/doc/html/Data-Aeson-Generic.html),
and is provided mainly for users of GHC older than 7.2.  It's far
slower (by about 10x) than the more modern generic mechanism.  To see
how to use SYB generics, see
[`examples/GenericSYB.hs`](https://github.com/bos/aeson/blob/master/examples/GenericSYB.hs).
