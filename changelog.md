0.8.0.2

* Fix ToJSON instance for 15-tuples (see #223).

0.8.0.1

* Support time-1.5.

0.8.0.0

* Add ToJSON and FromJSON instances for tuples of up to 15 elements.

0.7.1.0

* Major compiler and library compatibility changes: we have dropped
  support for GHC older than 7.4, text older than 1.1, and bytestring
  older than 0.10.4.0.  Supporting the older versions had become
  increasingly difficult, to the point where it was no longer worth
  it.

0.7.0.0

* The performance of encoding to and decoding of bytestrings have both
  improved by up to 2x, while also using less memory.

* New dependency: the scientific package lets us parse floating point
  numbers more quickly and accurately.

* eitherDecode, decodeStrictWith: fixed bugs.

* Added FromJSON and ToJSON instances for Tree and Scientific.

* Fixed the ToJSON instances for UTCTime and ZonedTime.

0.6 series

* Much improved documentation.

* Angle brackets are now escaped in JSON strings, to help avoid XSS
  attacks.

* Fixed up handling of nullary constructors when using generic
  encoding.

* Added ToJSON/FromJSON instances for:

  * The Fixed class
  * ISO-8601 dates: UTCTime, ZonedTime, and TimeZone

* Added accessor functions for inspecting Values.

* Added eitherDecode function that returns an error message if
  decoding fails.

0.5 to 0.6

* This release introduces a slightly obscure, but
  backwards-incompatible, change.

  In the generic APIs of versions 0.4 and 0.5, fields whose names
  began with a "_" character would have this character removed.  This
  no longer occurs, as it was both buggy and surprising
  (https://github.com/bos/aeson/issues/53).

* Fixed a bug in generic decoding of nullary constructors
  (https://github.com/bos/aeson/issues/62).

0.4 to 0.5

* When used with the UTF-8 encoding performance improvements
  introduced in version 0.11.1.12 of the text package, this release
  improves aeson's JSON encoding performance by 33% relative to aeson
  0.4.

  As part of achieving this improvement, an API change was necessary.
  The fromValue function in the Data.Aeson.Encode module now uses the
  text package's Builder type instead of the blaze-builder package's
  Builder type.

0.3 to 0.4

* The new decode function complements the longstanding encode
  function, and makes the API simpler.

* New examples make it easier to learn to use the package
  (https://github.com/bos/aeson/tree/master/examples).

* Generics support

  aeson's support for data-type generic programming makes it possible
  to use JSON encodings of most data types without writing any
  boilerplate instances.

  Thanks to Bas Van Dijk, aeson now supports the two major schemes for
  doing datatype-generic programming:

  * the modern mechanism, built into GHC itself
	(http://www.haskell.org/ghc/docs/latest/html/users_guide/generic-programming.html)

  * the older mechanism, based on SYB (aka "scrap your
	boilerplate")

  The modern GHC-based generic mechanism is fast and terse: in fact,
  its performance is generally comparable in performance to
  hand-written and TH-derived ToJSON and FromJSON instances.  To see
  how to use GHC generics, refer to examples/Generic.hs.

  The SYB-based generics support lives in Data.Aeson.Generic and is
  provided mainly for users of GHC older than 7.2.  SYB is far slower
  (by about 10x) than the more modern generic mechanism.  To see how
  to use SYB generics, refer to examples/GenericSYB.hs.

* We switched the intermediate representation of JSON objects from
  Data.Map to Data.HashMap which has improved type conversion
  performance.

* Instances of ToJSON and FromJSON for tuples are between 45% and 70%
  faster than in 0.3.

* Evaluation control

  This version of aeson makes explicit the decoupling between
  *identifying* an element of a JSON document and *converting* it to
  Haskell.  See the Data.Aeson.Parser documentation for details.

  The normal aeson decode function performs identification strictly,
  but defers conversion until needed.  This can result in improved
  performance (e.g. if the results of some conversions are never
  needed), but at a cost in increased memory consumption.

  The new decode' function performs identification and conversion
  immediately.  This incurs an up-front cost in CPU cycles, but
  reduces reduce memory consumption.
