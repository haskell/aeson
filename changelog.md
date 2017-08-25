For the latest version of this document, please see [https://github.com/bos/aeson/blob/master/changelog.md](https://github.com/bos/aeson/blob/master/changelog.md).

## Upcoming

* Fix handling of fractions when parsing Natural.
  Change text in error messages for Integral types to make them follow the common pattern.
  PVP level is "Non-breaking change".

## 1.2.1.0

* Add `parserThrowError` and `parserCatchError` combinators, thanks to Oleg Grenrus.

* Add `Generic` instance for `Value`, thanks to Xia Li-yao.

* Fix a mistake in the 1.2.0.0 changelog, the `cffi` flag is disabled by default! Thanks to dbaynard.

## 1.2.0.0

* `tagSingleConstructors`, an option to encode single-constructor types as tagged sums was added to `Options`. It is disabled by default for backward compatibility.

* The `cffi` flag is now turned off (`False`) by default, this means C FFI code is no longer used by default. You can flip the flag to get C implementation.

* The `Options` constructor is no longer exposed to prevent new options from being breaking changes, use `defaultOptions` instead.

* The contents of `GToJSON` and `GToEncoding` are no longer exposed.

* Some INLINE pragmas were removed to avoid GHC running out of simplifier ticks.

### 1.1.2.0

* Fix an accidental change in the format of `deriveJSON`. Thanks to Xia Li-yao!

* Documentation improvements regarding `ToJSON`, `FromJSON`, and `SumEncoding`. Thanks to Xia Li-yao and Lennart Spitzner!

### 1.1.1.0

* Added a pure implementation of the C FFI code, the C FFI code. If you wish to use the pure haskell version set the `cffi` flag to `False`. This should make aeson compile when C isn't available, such as for GHCJS. Thanks to James Parker & Marcin Tolysz!

* Using the `fast` flag can no longer cause a test case to fail. As far as we know this didn't affect any users of the library itself. Thanks to Xia Li-yao!

## 1.1.0.0

* Added instances for `UUID`.

* The operators for parsing fields now have named aliases:
  -  `.:` => `parseField`
  -  `.:?` => `parseFieldMaybe`
  -  `.:!` => `parseFieldMaybe'`
  - These functions now also have variants with explicit parser functions: `explicitParseField`, `explicitParseFieldMaybe`, "explicitParseFieldMaybe'`
Thanks to Oleg Grenrus.

* `ToJSONKey (Identity a)` and `FromJSONKey (Identity a)` no longer require the unnecessary `FromJSON a` constraint. Thanks to Oleg Grenrus.

* Added `Data.Aeson.Encoding.pair'` which is a more general version of `Data.Aeson.Encoding.pair`. Thanks to Andrew Martin.

* `Day`s BCE are properly encoded and `+` is now a valid prefix for `Day`s CE. Thanks to Matt Parsons.

* Some commonly used ToJSON instances are now specialized in order to improve compile time. Thanks to Bartosz Nitka.


[JSONTestSuite](https://github.com/nst/JSONTestSuite) cleanups, all
motivated by tighter RFC 7159 compliance:

* The parser now rejects numbers for which
  [the integer portion contains a leading zero](https://github.com/bos/aeson/commit/3fb7c155f2255482b1b9566ec5c1eaf9895d630e).
* The parser now rejects numbers for which
  [a decimal point is not followed by at least one digit](https://github.com/bos/aeson/commit/ecfca35a45286dbe2bbaf5f62354be393bc59b66),
* The parser now rejects documents that contain [whitespace outside the
  set {space, newline, carriage return, tab}](https://github.com/bos/aeson/commit/8ef622c2ad8d4a109884e17c2792238a2a320e44).

Over 90% of JSONTestSuite tests currently pass. The remainder can be
categorised as follows:

* The string parser is strict with Unicode compliance where the RFC
  leaves room for implementation-defined behaviour (tests prefixed
  with "`i_string_`". (This is necessary because the `text` library
  cannot accommodate invalid Unicode.)

* The parser does not (and will not) support UTF-16, UTF-32, or byte
  order marks (BOM).
* The parser accepts unescaped control characters, even though the RFC
  states that control characters must be escaped. (This may change at
  some point, but doesn't seem important.)

#### 1.0.2.1

* Fixes a regression where a bunch of valid characters caused an
  "Invalid UTF8-Stream" error when decoding. Thanks to Vladimir
  Shabanov who investigated and fixed this.

### 1.0.2.0

* Fixes a regression where it was no longer possible to derive
  instances for types such as `data T a = T { f1 :: a, f2 :: Maybe a
  }`.

Thanks to Sean Leather for fixing this, and to Ryan Scott for helping out.

### 1.0.1.0

* Decoding performance has been significantly improved (see
  https://github.com/bos/aeson/pull/452). Thanks to @winterland1989.

* Add `ToJSON`/`FromJSON` instances for newtypes from
  `Data.Semigroup`: `Min`, `Max`, `First`, `Last`, `WrappedMonoid`,
  `Option`. Thanks to Lennart Spitzner.

* Make the documentation for `.:!` more accurate. Thanks to Ian Jeffries.

# 1.0.0.0

Major enhancements:

* Introduced new `FromJSONKey` and `ToJSONKey` type classes that are
  used to encode maps without going through HashMap. This also allows arbitrary
  serialization of keys where a string-like key will encode into an object and
  other keys will encode into an array of key-value tuples.

* Added higher rank classes: `ToJSON1`, `ToJSON2`, `FromJSON1`, and
  `FromJSON2`.

* Added `Data.Aeson.Encoding` with functions to safely write `ToJSON`
  instances using `toEncoding`.

Other enhancements:

* A Cabal `fast` flag was added to disable building with optimizations. This drastically speeds up compiling both aeson ***and*** libraries using aeson so it is recommended to enable it during development. With cabal-install you can `cabal install aeson -ffast` and with stack you can add a flag section to your stack.yaml:
```
flags:
  aeson:
    fast: true
```

* Added list specific members to `ToJSON` and `FromJSON` classes. In
  the same way `Read` and `Show` handle lists specifically. This
  removes need for overlapping instances to handle `String`.

* Added a new `sumEncoding` option `UntaggedValue` which prevents
  objects from being tagged with the constructor name.

* JSONPaths are now tracked in instances derived with template-haskell
  and generics.

* Get rid of redundancy of JSONPath error messages in nested records.

  `eitherDecode "{\"x\":{\"a\": [1,2,true]}}" :: Either String Y`
  previously yielded
  `Error in $.x.a[2]: failed to parse field" x: failed to parse field a: expected Int, encountered Boolean`
  and now yields `Error in $.x.a[2]: expected Int, encountered Boolean"`.

  Some users might prefer to insert `modifyFailure` themselves to
  customize error messages, which previously prevented the use of
  `(.:)`.

* Backwards compatibility with `bytestring-0.9` using the
  `bytestring-builder` compatibility package.

* Export `decodeWith`, `decodeStrictWith`, `eitherDecodeWith`, and
  `eitherDecodeStrictWith` from `Data.Aeson.Parser`. This allows
  decoding using explicit parsers instead of using `FromJSON`
  instances.

* Un-orphan internal instances to render them in haddocks.

Other changes:

* Integral `FromJSON` instances now only accept integral
  values. E.g. parsing `3.14` to `Int` fails instead of succeeding
  with the value `3`.

* Over/underflows are now caught for bounded numeric types.

* Remove the `contents` field encoding with `allNullaryToStringTag = False`,
  giving us `{ "tag" : "c1" }` instead of `{ "tag" : "c1", contents : [] }`.
  The contents field is optional when parsing so this is only a breaking
  change for ToJSON instances.

* Fix a bug where `genericToEncoding` with `unwrapUnaryRecords = True`
  would produce an invalid encoding: `"unwrap\":""`.

* `ToJSON` instances using `genericToEncoding` and `omitNothingFields`
  no longer produce invalid JSON.

* Added instances for `DList`, `Compose`, `Product`, `Sum`.

### 0.11.2.0

* Enable `PolyKinds` to generalize `Proxy`, `Tagged`, and `Const` instances.
* Add `unsafeToEncoding` in `Data.Aeson.Types`, use with care!

#### 0.11.1.4

* Fix build with `base >= 4.8` and `unordered-containers < 0.2.6`.

#### 0.11.1.3

* Fix build on TH-less GHCs

#### 0.11.1.2

* Fix build with `base < 4.8` and `unordered-containers < 0.2.6`.
* Add missing field in docs for `defaultOptions`.

#### 0.11.1.1

* Fixes a bug where the hashes of equal values could differ.

### 0.11.1.0

The only changes are added instances.

These are new:
* `ToJSON a => ToJSON (NonEmpty a)`
* `FromJSON a => FromJSON (NonEmpty a)`
* `ToJSON (Proxy a)`
* `FromJSON (Proxy a)`
* `ToJSON b => ToJSON (Tagged a b)`
* `FromJSON b => FromJSON (Tagged a b)`
* `ToJSON a => ToJSON (Const a b)`
* `FromJSON a => FromJSON (Const a b)`

These are now available for older GHCs:
* `ToJSON Natural`
* `FromJSON Natural`

# 0.11.0.0

This release should be close to backwards compatible with aeson 0.9.

If you are upgrading from aeson 0.10 it might be easier to go back in
history to the point you were still using 0.9.

**Breaking changes**:

* Revert `.:?` to behave like it did in 0.9. If you want the 0.10
  behavior use `.:!` instead.

* Revert JSON format of `Either` to 0.9, `Left` and `Right` are now
  serialized with an initial uppercase letter. If you want the names
  in lowercase you can add a newtype with an instance.

* All `ToJSON` and `FromJSON` instances except for `[a]` are no longer
  `OVERLAPPABLE`. Mark your instance as `OVERLAPPING` if it overlaps
  any of the other aeson instances.

* All `ToJSON` and `FromJSON` instances except for `[Char]` are no
  longer incoherent, this means you may need to replace your
  incoherent instances with a newtyped instance.

**Additions**:

* Introduce `.:!` that behaves like `.:?` did in 0.10.

* Allow `HH:MM` format for `ZonedTime` and `UTCTime`.
  This is one of the formats allowed by
  [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Times).

* Added `ToJSON` and `FromJSON` instances for the
  `Version`, `Ordering`, and `Natural` types.

**Bug fixes**:

* JSONPath identifiers are now escaped if they contain invalid characters.

* Fixed JSONPath messages for Seq to include indices.

* Fixed JSONPath messages for Either to include `left`/`right`.

* Fix missing quotes surrounding time encodings.

* Fix #293: Type error in TH when using `omitNothingFields = True`.

**Compatibility**:

* Various updates to support GHC 8.


# 0.10.0.0

## Performance improvements

* Direct encoding via the new `toEncoding` method is over 2x faster
  than `toJSON`.  (You must write or code-gen a `toEncoding`
  implementation to unlock this speedup.  See below for details.)

* Improved string decoding gives a 12% speed win in parsing
  string-heavy JSON payloads (very common).

* Encoding and decoding of time-related types are 10x faster (!!) as a
  result of bypassing `Data.Time.Format` and the arbitrary-precision
  `Integer` type.

* When using `toEncoding`, `[Char]` can be encoded without a conversion to
  `Text`.  This is fast and efficient.

* Parsing into an `Object` is now 5% faster and more
  allocation-efficient.

## SUBTLE API CHANGES, READ CAREFULLY

With the exception of long-deprecated code, the API changes below
**should be upwards compatible** from older versions of `aeson`.  If you run
into upgrade problems, please file an issue with details.

* The `ToJSON` class has a new method, `toEncoding`, that allows
  direct encoding from a Haskell value to a lazy bytestring without
  construction of an intermediate `Value`.

  The performance benefits of direct encoding are significant: more
  than 2x faster than before, with less than 1/3 the memory usage.

  To preserve API compatibility across upgrades from older versions of
  this library, the default implementation of `toEncoding` uses
  `toJSON`.  You will *not* see any performance improvement unless you
  write an implementation of `toEncoding`, which can be very simple:

  ```haskell
  instance ToJSON Coord where
    toEncoding = genericToEncoding defaultOptions
  ```

  (Behind the scenes, the `encode` function uses `toEncoding` now, so
  if you implement `toEncoding` for your types, you should see a
  speedup immediately.)

  If you use Template Haskell or GHC Generics to auto-generate your
  `ToJSON` instances, you'll benefit from fast toEncoding
  implementations for free!

* When converting from a `Value` to a target Haskell type, `FromJSON`
  instances now provide much better error messages, including a
  complete JSON path from the root of the object to the offending
  element.  This greatly eases debugging.

* It is now possible to use Template Haskell to generate `FromJSON`
  and `ToJSON` instances for types in data families.

* If you use Template Haskell or generics, and used to use the
  `camelTo` function to rename fields, the new `camelTo2` function is
  smarter.  For example, `camelTo` will rename `CamelAPICase` to
  `camelapi_case` (ugh!), while `camelTo2` will map it to
  `camel_api_case` (yay!).

* New `ToJSON` and `FromJSON` instances for the following time-related
  types: `Day`, `LocalTime`.

* `FromJSON` `UTCTime` parser accepts the same values as for `ZonedTime`,
  but converts any time zone offset into a UTC time.

* The `Result` type is now an instance of `Foldable` and `Traversable`.

* The `Data.Aeson.Generic` module has been removed. It was deprecated in
  late 2013.

* GHC 7.2 and older are no longer supported.

* The instance of `Monad` for the `Result` type lacked an implementation
  of `fail` (oops).  This has been corrected.

* Semantics of `(.:?)` operator are changed. It's doesn't anymore accept
  present `Null` value.

* Added `(Foldable t, ToJSON a) => ToJSON (t a)` overlappable instance.
  You might see `No instance for (Foldable YourPolymorphicType) arising from a
  use of ‘.=’` -errors due this change.

# 0.9.0.1

* A stray export of `encodeToBuilder` got away!

# 0.9.0.0

* The `json` and `json'` parsers are now synonyms for `value` and
  `value'`, in conformance with the looser semantics of RFC 7159.

* Renamed `encodeToByteStringBuilder` to the more compact
  `encodeToBuilder`.

# 0.8.1.1

* The dependency on the `unordered-containers` package was too lax,
  and has been corrected.

# 0.8.1.0

* Encoding a `Scientific` value with a huge exponent is now handled
  efficiently.  (This would previously allocate a huge
  arbitrary-precision integer, potentially leading to a denial of
  service.)

* Handling of strings that contain backslash escape sequences is
  greatly improved.  For a pathological string containing almost a
  megabyte of consecutive backslashes, the new implementation is 27x
  faster and uses 42x less memory.

* The `ToJSON` instance for `UTCTime` is rendered with higher
  (picosecond) resolution.

* The `value` parser now correctly handles leading whitespace.

* New instances of `ToJSON` and `FromJSON` for `Data.Sequence` and
  `Data.Functor.Identity`.  The `Value` type now has a `Read` instance.

* `ZonedTime` parser ordering now favours the standard `JSON` format,
  increasing efficiency in the common case.

* Encoding to a `Text.Builder` now escapes `'<'` and `'>'` characters,
  to reduce XSS risk.

# 0.8.0.2

* Fix `ToJSON` instance for 15-tuples (see #223).

# 0.8.0.1

* Support `time-1.5`.

# 0.8.0.0

* Add `ToJSON` and `FromJSON` instances for tuples of up to 15
  elements.

# 0.7.1.0

* Major compiler and library compatibility changes: we have dropped
  support for GHC older than 7.4, `text` older than 1.1, and
  `bytestring` older than 0.10.4.0.  Supporting the older versions had
  become increasingly difficult, to the point where it was no longer
  worth it.

# 0.7.0.0

* The performance of encoding to and decoding of bytestrings have both
  improved by up to 2x, while also using less memory.

* New dependency: the `scientific` package lets us parse floating point
  numbers more quickly and accurately.

* `eitherDecode`, `decodeStrictWith`: fixed bugs.

* Added `FromJSON` and `ToJSON` instances for `Tree` and `Scientific`.

* Fixed the `ToJSON` instances for `UTCTime` and `ZonedTime`.

# 0.6 series

* Much improved documentation.

* Angle brackets are now escaped in JSON strings, to help avoid XSS
  attacks.

* Fixed up handling of nullary constructors when using generic
  encoding.

* Added `ToJSON`/`FromJSON` instances for:

  * The `Fixed` class
  * ISO-8601 dates: `UTCTime`, `ZonedTime`, and `TimeZone`

* Added accessor functions for inspecting `Value`s.

* Added `eitherDecode` function that returns an error message if
  decoding fails.

# 0.5 to 0.6

* This release introduces a slightly obscure, but
  backwards-incompatible, change.

  In the generic APIs of versions 0.4 and 0.5, fields whose names
  began with a `"_"` character would have this character removed.  This
  no longer occurs, as it was both buggy and surprising
  (https://github.com/bos/aeson/issues/53).

* Fixed a bug in generic decoding of nullary constructors
  (https://github.com/bos/aeson/issues/62).

# 0.4 to 0.5

* When used with the UTF-8 encoding performance improvements
  introduced in version 0.11.1.12 of the `text` package, this release
  improves `aeson`'s JSON encoding performance by 33% relative to
  `aeson` 0.4.

  As part of achieving this improvement, an API change was necessary.
  The `fromValue` function in the `Data.Aeson.Encode` module now uses
  the `text` package's `Builder` type instead of the `blaze-builder`
  package's `Builder` type.

# 0.3 to 0.4

* The new `decode` function complements the longstanding `encode`
  function, and makes the API simpler.

* New examples make it easier to learn to use the package
  (https://github.com/bos/aeson/tree/master/examples).

* Generics support

  `aeson`'s support for data-type generic programming makes it
  possible to use JSON encodings of most data types without writing
  any boilerplate instances.

  Thanks to Bas Van Dijk, `aeson` now supports the two major schemes
  for doing datatype-generic programming:

  * the modern mechanism, built into GHC itself
	(http://www.haskell.org/ghc/docs/latest/html/users_guide/generic-programming.html)

  * the older mechanism, based on SYB (aka "scrap your
	boilerplate")

  The modern GHC-based generic mechanism is fast and terse: in fact,
  its performance is generally comparable in performance to
  hand-written and TH-derived `ToJSON` and `FromJSON` instances.  To
  see how to use GHC generics, refer to `examples/Generic.hs`.

  The SYB-based generics support lives in `Data.Aeson.Generic` and is
  provided mainly for users of GHC older than 7.2.  SYB is far slower
  (by about 10x) than the more modern generic mechanism.  To see how
  to use SYB generics, refer to `examples/GenericSYB.hs`.

* We switched the intermediate representation of JSON objects from
  `Data.Map` to `Data.HashMap` which has improved type conversion
  performance.

* Instances of `ToJSON` and `FromJSON` for tuples are between 45% and 70%
  faster than in 0.3.

* Evaluation control

  This version of aeson makes explicit the decoupling between
  *identifying* an element of a JSON document and *converting* it to
  Haskell.  See the `Data.Aeson.Parser` documentation for details.

  The normal `aeson` `decode` function performs identification
  strictly, but defers conversion until needed.  This can result in
  improved performance (e.g. if the results of some conversions are
  never needed), but at a cost in increased memory consumption.

  The new `decode'` function performs identification and conversion
  immediately.  This incurs an up-front cost in CPU cycles, but
  reduces reduce memory consumption.
