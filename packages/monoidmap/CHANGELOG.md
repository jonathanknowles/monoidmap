# 0.1.1.0

- Moved the definitions of the `MonoidMap` type, its instances, and all of
  its functions from the `monoidmap-internal` package to `Data.MonoidMap`.
  The API of `Data.MonoidMap` is unchanged.

- Changed the data type name reported by the `Data` instance for
  `MonoidMap` from `"Data.MonoidMap.Internal.MonoidMap"` to
  `"Data.MonoidMap.MonoidMap"`.

- Revised `@since` annotations in the documentation to refer to versions of
  `monoidmap` rather than `monoidmap-internal`, and added annotations to all
  types, functions, and instances that lacked them.

- Fixed several broken links in the documentation.

- Moved the test suite here from the `monoidmap-internal` package.

# 0.1.0.0

- Changed the role of the value type parameter of `MonoidMap` from
  `representational` to `nominal`. This prevents coercions between
  `MonoidMap` types with different value types, which could produce maps
  that contain `mempty` values.

  This is a breaking change: code that performs such coercions will no
  longer compile.

# 0.0.6.1

- Improved the performance of `fromMap`, particularly for maps that
  contain no `mempty` values. This also benefits `fromList`, `fromListWith`
  and `fromSet`, which are defined in terms of `fromMap`.

# 0.0.6.0

- Added functions `nullifyKeysIn` and `nullifyKeysNotIn` to `MonoidMap`.

# 0.0.5.2

- Updated links to source repository in package metadata.

# 0.0.5.1

- Added support for GHC version `9.14`.
- Updated version bounds for dependencies.

# 0.0.5.0

- Made `MonoidMap` an instance of both `Typeable` and `Data`.

# 0.0.4.4

- Moved implementation, tests, and benchmark to the `monoidmap-internal`
  package.

# 0.0.4.3

- Moved all modules from `monoidmap-internal` to main library.

# 0.0.4.2

- Removed the dependency on `nonempty-containers`.

# 0.0.4.1

- Fixed spelling error in documentation.
- Added the haddock `not-home` marker to `Data.MonoidMap.Internal`.

# 0.0.4.0

- Added the `fromMapWith` function to `MonoidMap`.

# 0.0.3.0

- Added the `mapWithKey` function to `MonoidMap`.

# 0.0.2.1

- Added support for GHC 9.12.

# 0.0.2.0

- Added the `fromSet` function to `MonoidMap`.

# 0.0.1.9

- Added the following traversal functions to `MonoidMap`:
  - `traverse`
  - `traverseWithKey`
  - `mapAccumL`
  - `mapAccumLWithKey`
  - `mapAccumR`
  - `mapAccumRWithKey`

# 0.0.1.8

- Added strict variant of the `foldMapWithKey` function.

# 0.0.1.7

- Added a selection of folding operations for `MonoidMap`.

# 0.0.1.6

- Updated version bounds for dependencies.

# 0.0.1.5

- Updated version bounds for dependencies.

# 0.0.1.4

- Added support for GHC 9.10.

# 0.0.1.3

- Updated version bounds for dependencies.

# 0.0.1.2

- Updated version bounds for dependencies.

# 0.0.1.1

- Updated version bounds for dependencies.

# 0.0.1.0

- Added support for GHC 9.8.
- Optimised performance of `Semigroup.stimes` operation for `MonoidMap`.

# 0.0.0.1

- Revised `MultiMap` examples and documentation.

# 0.0.0.0

- Initial release.
