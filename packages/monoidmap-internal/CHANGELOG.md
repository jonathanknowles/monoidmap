# 0.1.2.0

- Improved the performance of `fromMap`, particularly for maps that
  contain no `mempty` values. This also benefits `fromList`, `fromListWith`
  and `fromSet`, which are defined in terms of `fromMap`.

# 0.1.1.0

- Added functions `nullifyKeysIn` and `nullifyKeysNotIn` to `MonoidMap`.

# 0.1.0.4

- Updated links to source repository in package metadata.

# 0.1.0.3

- Revised version bounds of dependencies.

# 0.1.0.2

- Added support for GHC `9.14`.
- Revised version bounds of dependencies.

# 0.1.0.1

- Revised version bounds of dependencies.

# 0.1.0.0

- Made `MonoidMap` an instance of both `Typeable` and `Data`.

# 0.0.0.1

- Revised version bounds of dependencies.

# 0.0.0.0

- Initial release.
