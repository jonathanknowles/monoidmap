# 0.2.0.0

- Changed the role of the value type parameter of `MonoidMap` from
  `representational` to `nominal`. This prevents coercions between
  `MonoidMap` types with different value types, which could produce maps
  that contain `mempty` values.

  This is a breaking change: code that performs such coercions will no
  longer compile.

- Moved the `RecoveredMap` module to the `monoidmap-examples` package.

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
