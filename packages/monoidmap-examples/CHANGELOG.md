# 0.1.0.0

- Changed the role of the value type parameter of `NestedMonoidMap` from
  `representational` to `nominal`. This is a consequence of the same change
  to `MonoidMap` in `monoidmap-0.1.0.0`.

  This is a breaking change: code that coerces between `NestedMonoidMap`
  types with different value types will no longer compile.

- Added the `RecoveredMap` module, which provides a re-implementation
  of `Data.Map.Strict.Map` in terms of `MonoidMap`. (This module was
  previously defined within the `monoidmap-internal` package.)

- Exported the `MultiSet` type from the `MultiSet` module.

- Revised version bounds for dependencies.

# 0.0.0.4

- Updated links to source repository in package metadata.

# 0.0.0.3

- Revised version bounds for dependencies.

# 0.0.0.2

- Added support for GHC version `9.14`.
- Revised version bounds for dependencies.

# 0.0.0.1

- Revised version bounds for dependencies.

# 0.0.0.0

- Initial release.
