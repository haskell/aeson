# 0.2.0.0 - 2026-05-21

- Accept 24:00:00 time of day.
- Fix parsers to reject years with more than 15 digits.

  This year parsing issue was previously reported as a DoS vulnerability in [HSEC-2026-0007](https://haskell.github.io/security-advisories/advisory/HSEC-2026-0007.html)
  but [we later reevaluated it as not a DoS vulnerability](https://github.com/haskell/security-advisories/issues/339).
  (The other vulnerability in aeson remains in that advisory.)
  Indeed, years were parsed in time `O(n log n)` which is asymptotically
  no slower than parsing integer literals (which happens on all JSON integers
  regardless of the target type, unlike dates).

# 0.1.1.2 - 2026-08-29 (backport from 0.2.0.0)

Backported from 0.2.0.0 to ease migration.

- Fix parsers to reject years with more than 15 digits.

# 0.1.1.1

- Support GHC-9.14

# 0.1.1

- Support GHC-8.6.5...9.12.1

# 0.1

Initial release
