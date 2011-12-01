# 0.3 to 0.4

## Ease of use

The new [`decode`
function](http://hackage.haskell.org/packages/archive/aeson/latest/doc/html/Data-Aeson.html#v:decode)
complements the longstanding `encode` function, and makes the API
simpler.

[New examples](https://github.com/bos/aeson/tree/master/examples) make
it easier to learn to use the package.


## Generics support

aeson's support for data-type generic programming makes it possible to
use JSON encodings of most data types without writing any boilerplate
instances.

Thanks to Bas Van Dijk, aeson now supports the two major schemes for
doing datatype-generic programming:

* the modern mechanism, [built into GHC
  itself](http://www.haskell.org/ghc/docs/latest/html/users_guide/generic-programming.html)

* the older mechanism, based on SYB (aka "scrap your boilerplate")

The modern GHC-based generic mechanism is fast and terse: in fact, its
performance is generally comparable in performance to hand-written and
TH-derived `ToJSON` and `FromJSON` instances.  To see how to use GHC
generics, refer to
[`examples/Generic.hs`](https://github.com/bos/aeson/blob/master/examples/Generic.hs).

The SYB-based generics support lives in
[Data.Aeson.Generic](http://hackage.haskell.org/packages/archive/aeson/latest/doc/html/Data-Aeson-Generic.html),
and is provided mainly for users of GHC older than 7.2.  SYB is far
slower (by about 10x) than the more modern generic mechanism.  To see
how to use SYB generics, refer to
[`examples/GenericSYB.hs`](https://github.com/bos/aeson/blob/master/examples/GenericSYB.hs).


## Improved performance

* We switched the intermediate representation of JSON objects from
  `Data.Map` to
  [`Data.HashMap`](http://hackage.haskell.org/package/unordered-containers),
  which has improved type conversion performance.

* Instances of `ToJSON` and `FromJSON` for tuples are between 45% and
  70% faster than in 0.3.
