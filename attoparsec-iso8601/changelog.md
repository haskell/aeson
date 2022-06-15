For the latest version of this document, please see [https://github.com/bos/aeson/blob/master/attoparsec-iso8601/changelog.md](https://github.com/bos/aeson/blob/master/attoparsec-iso8601/changelog.md).

### 1.1.0.0

- Change parsers of types with year (`Day`, `UTCTime`) to require years with at least 4 digits.
- Remove `fast` and `developer` package flags

### 1.0.2.1

* Code (re)organization.
* Avoid wildcard imports

### 1.0.2.0

* Add `month :: Parser Month` and `quarter :: Parser Quarter`

### 1.0.1.0

* Fixes handling of `UTCTime` wrt. leap seconds , thanks to Adam Schønemann
