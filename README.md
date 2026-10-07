# `monoidmap`

[![Development Branch](
  https://img.shields.io/badge/Development%20Branch-API%20Documentation-225577
)](https://jonathanknowles.github.io/monoidmap/)

This repository contains the [`monoidmap`][monoidmap-hackage] family of Haskell packages, built around the [`MonoidMap`] type:

> A [`MonoidMap`] represents a **total** function with **finite** support from keys to [monoidal][`Monoid`] values: **every** possible key is associated with a value, and only a **finite** number of keys are associated with values other than [`mempty`][`Monoid.mempty`].

For an extended introduction to the [`MonoidMap`] type, see the [README][monoidmap] for the `monoidmap` package.

## Packages

| Package<br>&nbsp; | Latest<br>Release | Description<br>&nbsp; |
|:--|:--:|:--|
| 📦 [`monoidmap`][monoidmap] | [![Latest Release][monoidmap-badge]][monoidmap-hackage] | Provides the core [`MonoidMap`] data type and functions. |
| 📦 [`monoidmap-examples`][monoidmap-examples] | [![Latest Release][monoidmap-examples-badge]][monoidmap-examples-hackage] | Provides worked examples of how to use [`MonoidMap`]. |
| 📦 [`monoidmap-aeson`][monoidmap-aeson] | [![Latest Release][monoidmap-aeson-badge]][monoidmap-aeson-hackage] | Provides support for JSON encoding with [`aeson`]. |
| 📦 [`monoidmap-hashable`][monoidmap-hashable] | [![Latest Release][monoidmap-hashable-badge]][monoidmap-hashable-hackage] | Provides support for in-memory hashing with [`hashable`]. |
| 📦 [`monoidmap-quickcheck`][monoidmap-quickcheck] | [![Latest Release][monoidmap-quickcheck-badge]][monoidmap-quickcheck-hackage] | Provides support for property testing with [`QuickCheck`]. |
| 📦 [`monoidmap-internal`][monoidmap-internal] | [![Latest Release][monoidmap-internal-badge]][monoidmap-internal-hackage] | Provides low-level internal functions. 🐉 |

[`MonoidMap`]: https://hackage-content.haskell.org/package/monoidmap/docs/Data-MonoidMap.html#g:1
[`Monoid`]: https://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Monoid
[`Monoid.mempty`]: https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:mempty
[`aeson`]: https://hackage.haskell.org/package/aeson
[`hashable`]: https://hackage.haskell.org/package/hashable
[`QuickCheck`]: https://hackage.haskell.org/package/QuickCheck

[monoidmap]: packages/monoidmap/README.md
[monoidmap-examples]: packages/monoidmap-examples/README.md
[monoidmap-aeson]: packages/monoidmap-aeson/README.md
[monoidmap-hashable]: packages/monoidmap-hashable/README.md
[monoidmap-quickcheck]: packages/monoidmap-quickcheck/README.md
[monoidmap-internal]: packages/monoidmap-internal/README.md

[monoidmap-hackage]: https://hackage.haskell.org/package/monoidmap
[monoidmap-examples-hackage]: https://hackage.haskell.org/package/monoidmap-examples
[monoidmap-aeson-hackage]: https://hackage.haskell.org/package/monoidmap-aeson
[monoidmap-hashable-hackage]: https://hackage.haskell.org/package/monoidmap-hashable
[monoidmap-quickcheck-hackage]: https://hackage.haskell.org/package/monoidmap-quickcheck
[monoidmap-internal-hackage]: https://hackage.haskell.org/package/monoidmap-internal

[monoidmap-badge]: https://img.shields.io/hackage/v/monoidmap?label=&color=b74040
[monoidmap-examples-badge]: https://img.shields.io/hackage/v/monoidmap-examples?label=&color=905f33
[monoidmap-aeson-badge]: https://img.shields.io/hackage/v/monoidmap-aeson?label=&color=766a2a
[monoidmap-hashable-badge]: https://img.shields.io/hackage/v/monoidmap-hashable?label=&color=2a774a
[monoidmap-quickcheck-badge]: https://img.shields.io/hackage/v/monoidmap-quickcheck?label=&color=386da1
[monoidmap-internal-badge]: https://img.shields.io/hackage/v/monoidmap-internal?label=&color=8e48bf
