Parsing of ISO 8601 dates.

This package is used to parse dates in aeson. It is split into a
separate package to be shared by other projects that want to parse
dates like aeson does.

For now, this project is located in the aeson repository and aeson
itself uses the source of this package without pulling in the package
as a dependency.

## Stability

Since aeson depends on this package we want to be very careful about
changing the format.

There may be breaking changes if we find that the format is
incorrectly too lenient. We consider widening of the allowed input a
non-breaking addition since all previously valid input will still
parse correctly.
