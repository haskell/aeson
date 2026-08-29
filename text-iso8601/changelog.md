# 0.1.1.2 - 2026-08-29

Fix a DoS vulnerability caused by parsing large numbers (advisory [HSEC-2026-0007](https://haskell.github.io/security-advisories/advisory/HSEC-2026-0007.html)). Backported from 0.2.0.0 to ease migration.

- Fix parsers to reject years with more than 15 digits

# 0.1.1.1

- Support GHC-9.14

# 0.1.1

- Support GHC-8.6.5...9.12.1

# 0.1

Initial release
