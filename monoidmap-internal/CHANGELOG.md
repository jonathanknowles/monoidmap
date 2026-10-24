# 0.4.0.0

- Revised the module structure of the package, replacing the
  `Data.MonoidMap.Internal` module with the following modules:

  - `Data.MonoidMap.Internal.NonNull`
  - `Data.MonoidMap.Internal.NonNullMap`
  - `Data.MonoidMap.Internal.NonNullMap.FromMap`
  - `Data.MonoidMap.Internal.NonNullMap.FromMapWith`

  The new structure has the following aims:

  - To confine the data constructor of `NonNull` to two small private
    modules, making the code that maintains the `NonNull` invariant
    a little easier to audit.

  - To dedicate each public module to a single type, with functions named
    for qualified import. Several functions have been renamed accordingly:
    for example, `maybeNonNull` is now `NonNull.nonNull`, and
    `fromNonNullMap` is now `NonNullMap.toMap`.

  - To move the alternative `fromMap*` and `fromMapWith*` implementations,
    which exist only for benchmarking, into their own modules, so that
    they no longer clutter the main API.

- Changed `fromMap`, `fromMapWith`, and `toMap` to accept or return values
  of type `NonNullMap k v` instead of `Map k (NonNull v)`.

- Added documentation for the `NonNull` and `NonNullMap` types and their
  functions.

# 0.3.0.0

- Moved the `MonoidMap` type, its instances, and all of its functions to
  the `Data.MonoidMap` module of the `monoidmap` package. These are no
  longer exported by `Data.MonoidMap.Internal`.

- Removed the `Data.MonoidMap.Internal.Unsafe` module, including the
  `unsafeFromMap` function.

- Changed `fromMap`, `fromMapWith`, and their alternative implementations
  to return values of type `Map k (NonNull v)` instead of `MonoidMap k v`.

- Stopped exporting the data constructor of `NonNull`, and removed its
  `getNonNull` field.

- Changed the role of the type parameter of `NonNull` from
  `representational` to `nominal`.

- Added the `NonNullMap` type, and exported the `maybeNonNull`,
  `applyNonNull`, `applyNonNull2`, and `fromNonNullMap` functions.

- Made `NonNull` an instance of `NFData`.

- Moved the test suite to the `monoidmap` package.

- Removed the dependency on `groups`.

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
