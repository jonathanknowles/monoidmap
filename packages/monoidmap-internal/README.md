# `monoidmap`
<a href="https://jonathanknowles.github.io/monoidmap/"><img src="https://img.shields.io/badge/API-Documentation-227755" /></a>

# Overview

This library provides a **[`MonoidMap`]** type that:

- models a [total function](#relationship-between-keys-and-values) with [finite support](https://en.wikipedia.org/wiki/Support_(mathematics)) from keys to [monoidal][`Monoid`] values, with a default value of [`mempty`][`Monoid.mempty`].
- encodes key-value mappings with a [minimal encoding](#encoding) that only
includes values _not_ equal to [`mempty`][`Monoid.mempty`].
- provides a comprehensive set of [monoidal operations](#monoidal-operations) for transforming, combining, and comparing maps.
- provides a [general basis](#General-basis-for-more-specialised-map-types) for building more specialised monoidal data structures.

# Relationship between keys and values

A map of type <code><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#t:MonoidMap">MonoidMap</a> k v</code> associates **every** possible key of type `k` with a value of type `v`:

```hs
MonoidMap.get :: (Ord k, Monoid v) => k -> MonoidMap k v -> v
```

The [`empty`][`MonoidMap.empty`] map associates every key `k` with a default value of [`mempty`][`Monoid.mempty`]:

```hs
∀ k. MonoidMap.get k MonoidMap.empty == mempty
```

## Comparison with standard `Map` type

The [`MonoidMap`] type differs from the standard [`containers`] [`Map`] type in how it relates keys to values:

|            Type | Models a total function with finite support        |
|----------------:|:---------------------------------------------------|
|       `Map k v` | from keys of type `k` to values of type `Maybe v`. |
| `MonoidMap k v` | from keys of type `k` to values of type `v`.       |

This difference can be illustrated by comparing the type signatures of operations to query a key for its value, for both types:

```hs
      Map.lookup ::             k ->       Map k v -> Maybe v
MonoidMap.get    :: Monoid v => k -> MonoidMap k v ->       v
```

Whereas a standard [`Map`] has a default value of [`Nothing`], a [`MonoidMap`] has a default value of [`mempty`][`Monoid.mempty`]:

```hs
∀ k.       Map.lookup k       Map.empty == Nothing
∀ k. MonoidMap.get    k MonoidMap.empty == mempty
```

In practice, the standard [`Map`] type uses [`Maybe`] to indicate the _presence_ or _absence_ of a value for a particular key. This representation is necessary because the [`Map`] type imposes no constraints on value types.

However, _monoidal_ types already have a natural way to represent null or empty values: the [`mempty`][`Monoid.mempty`] constant, which represents the neutral or identity element of a [`Monoid`].

Consequently, using a standard [`Map`] with a _monoidal_ value type gives rise to _two_ distinct representations for null or empty values:

| `Map.lookup k m` | Interpretation                                              |
|:-----------------|:------------------------------------------------------------|
| `Nothing`        | Map `m` has _no_ entry for key `k`.                         |
| `Just mempty`    | Map `m` has an entry for key `k`, but the value is _empty_. |

In contrast, the [`MonoidMap`] type provides a single, _canonical_ representation for null or empty values, according to the following conceptual mapping:

| `Map.lookup k m`        | ⟼ | `MonoidMap.get k m`     |
|:------------------------|---|:------------------------|
| `Nothing`               | ⟼ | `mempty`                |
| `Just v \| v == mempty` | ⟼ | `mempty`                |
| `Just v \| v /= mempty` | ⟼ | `v`                     |

## Advantages of using a canonical representation

A canonical representation for [`mempty`][`Monoid.mempty`] values can make it easier to correctly implement operations that compare or combine pairs of maps.

When comparing or combining maps of the standard [`containers`] [`Map`] type, there are **two** cases to consider for each key `k` in each map:

- [`Map`] `m` associates `k` with `Nothing`.
- [`Map`] `m` associates `k` with `Just v`.

With a _pair_ of maps, there are **four** possible cases to consider for each key.

For maps with monoidal values, and in contexts that assume or require a default value of [`mempty`][`Monoid.mempty`], there are now **three** cases to consider for each map:

- [`Map`] `m` associates `k` with `Nothing`.
- [`Map`] `m` associates `k` with `Just v` where `v == mempty`.
- [`Map`] `m` associates `k` with `Just v` where `v /= mempty`.

With a _pair_ of maps, there are now **nine** possible cases to consider for each key.

Mishandling cases such as these can give rise to subtle bugs that manifest in unexpected places. For maps with more complex value types (such as maps that nest other maps), the number of cases requiring consideration can easily multiply further, making it even easier to introduce bugs.

Since all [`MonoidMap`] operations provide a canonical representation for [`mempty`][`Monoid.mempty`] values, it's possible to write functions that compare or combine maps without having to consider [`Nothing`] and <code><a href="https://hackage.haskell.org/package/base/docs/Data-Maybe.html#v:Just">Just</a> <a href="https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:mempty">mempty</a></code> as separate cases.

# Encoding

A [`MonoidMap`] only encodes mappings from keys to values that are **_not_** equal to [`mempty`][`Monoid.mempty`].

The total function $T$ modelled by a [`MonoidMap`] is encoded as a **support map** $S$, where $S$ is the finite subset of key-value mappings in $T$ for which values are **_not_** equal to [`mempty`][`Monoid.mempty`] (denoted by $\varnothing$):

> $S = \\{ \(k, v\) \in T \ \|\ v \ne \varnothing \\} $

## Automatic minimisation

All [`MonoidMap`] operations perform **automatic minimisation** of the support map, so that [`mempty`][`Monoid.mempty`] values do not appear in:
- any encoding of a [`MonoidMap`];
- any traversal of a [`MonoidMap`].

## Constraints on values

[`MonoidMap`] operations require the monoidal value type to be an instance of [`MonoidNull`].

Instances of [`MonoidNull`] must provide a [`null`][`MonoidNull.null`] indicator function that satisfies the following law:

```hs
null v == (v == mempty)
```

[`MonoidMap`] operations use the [`null`][`MonoidNull.null`] indicator function to detect and exclude [`mempty`][`Monoid.mempty`] values from the support map.

Note that it is _not_ generally necessary for the value type to be an instance of [`Eq`].

<details><summary><strong>Justification</strong></summary>
<br/>

> The set of monoidal types that admit a [`MonoidNull`] instance is strictly larger than the set of monoidal types that admit an [`Eq`] instance.
>
> For any type `v` that is an instance of both [`Eq`] and [`Monoid`], it is _always_ possible to define a [`MonoidNull`] instance:
>
> ```hs
> instance MonoidNull v where
>     null = (== mempty)
> ```
>
> However, there are monoidal types for which it is possible to define a [`MonoidNull`] instance, but not practical (or possible) to define a lawful [`Eq`] instance.
>
> For example, consider the following type:
> ```hs
> Maybe (String -> Sum Natural)
> ```
>
> Requiring a [`MonoidNull`] constraint instead of an [`Eq`] constraint allows [`MonoidMap`] to be usable with a greater range of monoidal value types.

</details>

## Examples of automatic minimisation

<details><summary><strong>Updating a map with a value that <em>may</em> be <code>mempty</code></strong></summary>
<br/>

> Consider the following operation, which constructs a map of type `MonoidMap Int String`:
>
> ```hs
> >>> m0 = fromList [(1, "hello"), (2, "brave"), (3, "new"), (4, "world")]
> >>> m0
> fromList [(1, "hello"), (2, "brave"), (3, "new"), (4, "world")]
> ```
>
> The [`Monoid`] instance for [`String`] defines [`mempty`][`Monoid.mempty`] to be the empty [`String`] `""`.
>
> If we update the map to associate key `3` with value `""`, that association will no longer appear when encoding the map:
>
> ```hs
> >>> m1 = MonoidMap.set 3 "" m0
> >>> m1
> fromList [(1, "hello"), (2, "brave"), (4, "world")]
> ```
>
> However, we can still read the updated value for key `3`:
> ```hs
> >>> MonoidMap.get 3 m1
> ""
> ```

</details>

<details><summary><strong>Constructing a map from a list that <em>may</em> include <code>mempty</code> values</strong></summary>
<br/>

> Consider the following operation, which constructs a map of type `MonoidMap Char (Sum Natural)`:
>
> ```hs
> >>> m = fromList [('a', Sum 0), ('b', Sum 1), ('c', Sum 2), ('d', Sum 3)]
> ```
> The [`Monoid`] instance for <code><a href="https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:Sum">Sum</a> <a href="https://hackage.haskell.org/package/base/docs/Numeric-Natural.html#t:Natural">Natural</a></code> defines [`mempty`][`Monoid.mempty`] to be `Sum 0`.
>
> The original list contained a mapping from key `'a'` to value `Sum 0`, but that association will not appear when encoding the map:
>
> ```hs
> >>> m
> fromList [('b', Sum 1), ('c', Sum 2), ('d', Sum 3)]
> ```
>
> Nevertheless, we can still read the value for key `'a'`:
> ```hs
> >>> MonoidMap.get 'a' m
> Sum 0
> ```

</details>

<details><summary><strong>Combining maps with operations that <em>may</em> produce <code>mempty</code> values</strong></summary>
<br/>

> Consider the following operations, which construct two maps of type `MonoidMap Char (Sum Natural)`:
>
> ```hs
> >>> m1 = fromList [('a', Sum 1), ('b', Sum   1 )]
> >>> m2 = fromList [('a', Sum 1), ('b', Sum (-1))]
> ```
>
> The [`Semigroup`] instance for <code><a href="https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:Sum">Sum</a> <a href="https://hackage.haskell.org/package/base/docs/Numeric-Natural.html#t:Natural">Natural</a></code> defines [`<>`] as equivalent to ordinary addition.
>
> If we add both maps together with [`<>`], then each key in the resulting map will be associated with the result of applying [`<>`] to each matching pair of values in the original maps. However, adding together the values for key `'b'` with [`<>`] produces `Sum 0`, so this value will not appear in the encoding:
>
> ```hs
> >>> m1 <> m2
> fromList [('a', Sum 2)]
> ```
>
> Nevertheless, we can still read the value for key `'b'`:
> ```hs
> >>> MonoidMap.get 'b' (m1 <> m2)
> Sum 0
> ```

</details>

## Advantages of automatic minimisation

### Consistency

Automatic exclusion of [`mempty`][`Monoid.mempty`] values can help to ensure consistency when encoding to or decoding from other formats such as JSON, CBOR, or YAML.

For example, you may wish to ensure that:

- When _encoding_ a map, no [`mempty`][`Monoid.mempty`] values appear in the encoded result.
- When _decoding_ a map, no [`mempty`][`Monoid.mempty`] values appear in the decoded result.

### Performance

Automatic exclusion of [`mempty`][`Monoid.mempty`] values makes it possible to perform certain operations in _constant time_, rather than in linear time, as it is never necessary to traverse the entire map in order to determine which values may or may not be [`mempty`][`Monoid.mempty`]:

<table>
<thead>
  <tr valign="top" align="left">
    <th>Operation</th>
    <th>With<br>Automatic<br>Minimisation</th>
    <th>Without<br>Automatic<br>Minimisation</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:null" rel="nofollow"><code>null</code></a></td>
    <td>$O(1)$</td>
    <td>$O(n)$</td>
  </tr>
  <tr>
    <td><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:nonNull" rel="nofollow"><code>nonNull</code></a></td>
    <td>$O(1)$</td>
    <td>$O(n)$</td>
  </tr>
  <tr>
    <td><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:nonNullCount" rel="nofollow"><code>nonNullCount</code></a></td>
    <td>$O(1)$</td>
    <td>$O(n)$</td>
  </tr>
  <tr>
    <td><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:toMap" rel="nofollow"><code>toMap</code></a></td>
    <td>$O(1)$</td>
    <td>$O(n)$</td>
  </tr>
</tbody>
</table>

### Memory usage

Automatic minimisation makes it easier to reason about the memory usage of a [`MonoidMap`], as memory is not required to encode mappings from keys to empty values.

This is a useful property for large, long-lived map structures that are subject to multiple updates over their lifetimes, where it's important to not retain large numbers of mappings from keys to empty values.

### Simplicity

Some total map data types only perform minimisation when _explicitly demanded to_.

For example, the [`TMap`] data type provides a [`trim`][`TMap.trim`] operation that traverses the map and removes any values that are equal to the _default_ value. This approach has some advantages, such the ability to provide a lawful [`Functor`] instance.

However, this approach also has some disadvantages:
- It might not be obvious _when_ it's necessary to call [`trim`][`TMap.trim`]. For example, consider the following operation: `m1 <> m2`. Could this operation produce a map where some keys map to default values? (Answer: it depends on the choice of default value and the underlying value type.)
- Calling [`trim`][`TMap.trim`] when it _isn't_ necessary might adversely affect performance, since [`trim`][`TMap.trim`] must traverse the entire data structure.
- Not calling [`trim`][`TMap.trim`] when it _is_ necessary might affect correctness. The compiler will not help here, as trimmed and untrimmed maps share the same type.
- Even if [`trim`][`TMap.trim`] is a semantic no-op, default values can _still_ be made visible by operations that encode maps to other types.

Since all [`MonoidMap`] operations perform automatic minimisation when appropriate, it's not necessary for users to reason about when or whether it's necessary to "trim" the map.

Furthermore, for nested maps such as <code><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#t:MonoidMap">MonoidMap</a> k1 (<a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#t:MonoidMap">MonoidMap</a> k2 v)</code>, automatic minimisation of inner maps enables seamless automatic minimisation of outer maps. See the [`NestedMonoidMap`] type for an example of this.

## Limitations of automatic minimisation

The [`MonoidMap`] type has no [`Functor`] instance, as the requirement to exclude [`mempty`][`Monoid.mempty`] values means that the [`map`][`MonoidMap.map`] operation must remove [`mempty`][`Monoid.mempty`] values from its result. Therefore, [`map`][`MonoidMap.map`] does _not_ unconditionally satisfy the functor composition law:

```hs
map (f . g) == map f . map g
```

<details><summary><strong>Example violation</strong></summary>
<br/>

Consider the following [`MonoidMap`] `m`:
```hs
m :: MonoidMap String String
m = singleton "k" "v"
```

And the following functions `f` and `g`:
```hs
f :: a -> String
f = const "z"

g :: Monoid a => b -> a
g = const mempty
```

By substituting the above definitions into the left-hand side of the functor composition law, we obtain:
```hs
map (f . g) m = map (const "z" . const mempty) (singleton "k" "v")
              = map (const "z"               ) (singleton "k" "v")
              =                                (singleton "k" "z")
```

By substituting the above definitions into the right-hand side of the functor composition law, we obtain:
```hs
map f (map g m) = map (const "z") (map (const mempty) (singleton "k" "v"))
                = map (const "z") mempty
                =                 mempty
```

This leads to the following inequality between the left-hand side and right-hand side:
```hs
singleton "k" "z" /= mempty
```
Therefore, for this example, the functor composition law is not satisfied.

</details>

However, if applying function `f` to [`mempty`][`Monoid.mempty`] produces [`mempty`][`Monoid.mempty`], the functor composition law is satisfied:

```hs
(f mempty == mempty) ==> (∀ g. map (f . g) == map f . map g)
```

# Monoidal operations

The [`MonoidMap`] type provides a comprehensive set of monoidal operations for transforming, combining, and comparing maps.

Instances for several _subclasses_ of [`Semigroup`] and [`Monoid`] are provided, including classes from the following libraries:

- [`monoid-subclasses`]
- [`groups`]

At the root of this hierarchy of subclasses is the [`Semigroup`] class, whose instance for [`MonoidMap`] is defined in terms of the _underlying value type_, so that applying [`<>`] to a _pair of maps_ is equivalent to applying [`<>`] to all _pairs of values_ for matching keys:

```hs
∀ k. MonoidMap.get k (m1 <> m2) == MonoidMap.get k m1 <> get k m2
```

In general, operations for subclasses of [`Semigroup`] and [`Monoid`] are defined _analogously_ to the [`Semigroup`] instance, so that:

- _unary_ operations on _individual maps_ are defined in terms of their distributive application to all values.
- _binary_ operations on _pairs of maps_ are defined in terms of their distributive application to all _pairs of values_ for matching keys.

Unary monoidal operations typically satisfy a property similar to:

```hs
∀ k. MonoidMap.get k (f m) == f (MonoidMap.get k m)
```

Binary monoidal operations typically satisfy a property similar to:

```hs
∀ k. MonoidMap.get k (f m1 m2) == f (MonoidMap.get k m1) (MonoidMap.get k m2)
```

Defining monoidal operations in this way makes it possible to transform, combine, and compare maps in ways that are consistent with the algebraic properties of the underlying monoidal value type.

## Examples of monoidal operations and their properties

<table>
<thead>
  <tr valign="top" align="left">
    <th><code>MonoidMap</code><br>operation</th>
    <th>Required<br>class<br>constraint</th>
    <th>Equivalent<br>class<br>method</th>
    <th>Distributive<br>relationship</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:append"><code>append</code></a></td>
    <td><a href="https://hackage.haskell.org/package/base/docs/Data-Semigroup.html#t:Semigroup"><code>Semigroup</code></a></td>
    <td><a href="https://hackage.haskell.org/package/base/docs/Data-Semigroup.html#v:-60--62-"><code>(&lt;&gt;)</code></a></td>
    <td><code>∀ k. <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k (m1  <a href="https://hackage.haskell.org/package/base/docs/Data-Semigroup.html#v:-60--62-"><></a>   m2) ≡ <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k m1  <a href="https://hackage.haskell.org/package/base/docs/Data-Semigroup.html#v:-60--62-"><></a>   <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k m2</code></td>
  </tr>
  <tr>
    <td><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:minus"><code>minus</code></a></td>
    <td><a href="https://hackage.haskell.org/package/groups/docs/Data-Group.html#t:Group"><code>Group</code></a></td>
    <td><a href="https://hackage.haskell.org/package/groups/docs/Data-Group.html#v:-126--126-"><code>(~~)</code></a></td>
    <td><code>∀ k. <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k (m1  <a href="https://hackage.haskell.org/package/groups/docs/Data-Group.html#v:-126--126-">~~</a>   m2) ≡ <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k m1  <a href="https://hackage.haskell.org/package/groups/docs/Data-Group.html#v:-126--126-">~~</a>   <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k m2</code></td>
  </tr>
  <tr>
    <td><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:monus"><code>monus</code></a></td>
    <td><a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Monus.html#t:Monus"><code>Monus</code></a></td>
    <td><a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Monus.html#v:-60--92--62-"><code>(&lt;\&gt;)</code></a></td>
    <td><code>∀ k. <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k (m1  <a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Monus.html#v:-60--92--62-"><\></a>  m2) ≡ <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k m1  <a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Monus.html#v:-60--92--62-"><\></a>  <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k m2</code></td>
  </tr>
  <tr>
    <td><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:intersection"><code>intersection</code></a></td>
    <td><a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-GCD.html#t:GCDMonoid"><code>GCDMonoid</code></a></td>
    <td><a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-GCD.html#v:gcd"><code>gcd</code></a></td>
    <td><code>∀ k. <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k (m1 `<a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-GCD.html#v:gcd">gcd</a>` m2) ≡ <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k m1 `<a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-GCD.html#v:gcd">gcd</a>` <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k m2</code></td>
  </tr>
  <tr>
    <td><a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:union"><code>union</code></a></td>
    <td><a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-LCM.html#t:LCMMonoid"><code>LCMMonoid</code></a></td>
    <td><a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-LCM.html#v:lcm"><code>lcm</code></a></td>
    <td><code>∀ k. <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k (m1 `<a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-LCM.html#v:lcm">lcm</a>` m2) ≡ <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k m1 `<a href="https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-LCM.html#v:lcm">lcm</a>` <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:get">get</a> k m2</code></td>
  </tr>
</tbody>
</table>

## Examples of monoidal operations applied to values

<details><summary><strong>Example: <code>MonoidMap k (Set Integer)</code></strong></summary>
<br/>

For maps with [`Set`]-based values, [`MonoidMap.union`] and [`MonoidMap.intersection`] compute the [`Set.union`] and [`Set.intersection`] of each pair of matching values, respectively.

Consider the following maps of type `MonoidMap Char (Set Integer)`:

```hs
>>> m1 = fromList [('a', Set.fromList [0, 1]), ('b', Set.fromList [3, 4])]
>>> m2 = fromList [('a', Set.fromList [0, 2]), ('b', Set.fromList [3, 5])]
```

The [`MonoidMap.union`] of maps `m1` and `m2` is a map that associates every key `k` with the [`Set.union`] of the corresponding sets for `k` in `m1` and `m2`:

```hs
>>> m1 `union` m2
fromList [('a', Set.fromList [0,1,2]), ('b', Set.fromList [3,4,5])]
```

The [`MonoidMap.intersection`] of maps `m1` and `m2` is a map that associates every key `k` with the [`Set.intersection`] of the corresponding sets for `k` in `m1` and `m2`:

```hs
>>> m1 `intersection` m2
fromList [('a', Set.fromList [0]), ('b', Set.fromList [3])]
```
</details>

<details><summary><strong>Example: <code>MonoidMap k (Sum Integer)</code></strong></summary>
<br/>

Consider the following maps of type `MonoidMap Char (Sum Integer)`:

```hs
>>> m1 = fromList [('a', Sum 10), ('b', Sum 20), ('c, Sum 40)]
>>> m2 = fromList [('a', Sum 40), ('b', Sum 20), ('c, Sum 10)]
```

The [`MonoidMap.invert`] operation produces a new map where every key is associated with the negation of its value in the original map:

```hs
>>> invert m1
fromList [('a', Sum (-10)), ('b', Sum (-20)), ('c, Sum (-40))]

>>> invert m2
fromList [('a', Sum (-40)), ('b', Sum (-20)), ('c, Sum (-10))]
```

The [`MonoidMap.minus`] operation, when applied to maps `m1` and `m2`, produces a new map where every key `k` is associated with the value of `k` in `m1` minus the value of `k` in `m2`:

```hs
>>> m1 `minus` m2
fromList [('a', Sum (-30)), ('c', Sum 30)]

>>> m2 `minus` m1
fromList [('a', Sum 30), ('c', Sum (-30))]
```

</details>

<details><summary><strong>Example: <code>MonoidMap k (Sum Natural)</code></strong></summary>
<br/>

For maps with <code><a href="https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:Sum">Sum</a> <a href="https://hackage.haskell.org/package/base/docs/Numeric-Natural.html#t:Natural">Natural</a></code> values, [`MonoidMap.union`] and [`MonoidMap.intersection`] compute the _maximum_ and _minimum_ of each pair of matching values, respectively:

```hs
>>> m1 = fromList [('a', Sum 10), ('b', Sum 20)]
>>> m2 = fromList [('a', Sum 20), ('b', Sum 10)]

>>> m1 `union` m2
fromList [('a', Sum 20), ('b', Sum 20)]

>>> m1 `intersection` m2
fromList [('a', Sum 10), ('b', Sum 10)]
```
</details>

<details><summary><strong>Example: <code>MonoidMap k (Product Natural)</code></strong></summary>
<br/>

For maps with <code><a href="https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:Product">Product</a> <a href="https://hackage.haskell.org/package/base/docs/Numeric-Natural.html#t:Natural">Natural</a></code> values, [`MonoidMap.union`] and [`MonoidMap.intersection`] compute the _lowest common multiple_ (LCM) and _greatest common divisor_ (GCD) of each pair of matching values, respectively:

```hs
>>> m1 = fromList [('a', Product  6), ('b', Product 15), ('c', Product 35)]
>>> m2 = fromList [('a', Product 15), ('b', Product 35), ('c', Product 77)]

>>> m1 `union` m2
fromList [('a', Product 30), ('b', Product 105), ('c', Product 385)]

>>> m1 `intersection` m2
fromList [('a', Product 3), ('b', Product 5), ('c', Product 7)]
```
</details>

# General basis for more specialised map types

The [`MonoidMap`] type can be used as a general basis for building other more specialised map types.

If you have a [`Map`]-based data type with an invariant that values **must not** be [`mempty`][`Monoid.mempty`], then by expressing this type in terms of [`MonoidMap`], [`MonoidMap`] will handle the invariant for you:

```patch
- newtype SomeMap k v = SomeMap (      Map k (SomeMonoidalContainer v))
+ newtype SomeMap k v = SomeMap (MonoidMap k (SomeMonoidalContainer v))
```

If you're already using a specialised non-empty container type to enforce the invariant that values must not be empty, then [`MonoidMap`] makes it possible to _replace_ the use of the specialised non-empty container type with its ordinary equivalent:

Example transformations:
```patch
  -- Non-empty lists:
- newtype ListMap k v = ListMap (      Map k (NonEmpty v))
+ newtype ListMap k v = ListMap (MonoidMap k          [v])

  -- Non-empty sets:
- newtype SetMap k v = SetMap (      Map k (NonEmptySet v))
+ newtype SetMap k v = SetMap (MonoidMap k         (Set v))

  -- Non-empty sequences:
- newtype SeqMap k v = SeqMap (      Map k (NonEmptySeq v))
+ newtype SeqMap k v = SeqMap (MonoidMap k         (Seq v))
```

Using [`MonoidMap`] can simplify the implementation of such types, as special handling code for empty values can often be greatly simplified or even eliminated.

## Real-world examples from the Haskell ecosystem

### Example: `SignedMultiSet` (a signed multiset type)

> The [`signed-multiset`] library provides the [`SignedMultiSet`] type, which is internally defined as a [`Map`] from elements to signed integer occurrence counts:
>
> ```hs
> newtype SignedMultiset a = SMS {unSMS :: Map a Int}
> ```
>
> All [`SignedMultiSet`] operations maintain an invariant that the internal [`Map`] **must not** contain any mappings to `0` (zero). This requires [`SignedMultiSet`] functions to detect and eliminate values of `0`.
>
> For example, the [`insertMany`][`SignedMultiSet.insertMany`] operation:
>
> ```hs
> insertMany :: Ord a => a -> Int -> SignedMultiset a -> SignedMultiset a
> insertMany x n = SMS . Map.alter f x . unSMS
>   where
>     f Nothing  = Just n
>     f (Just m) = let k = m + n in if k == 0 then Nothing else Just k
> ```
>
> Let's redefine [`SignedMultiSet`] in terms of [`MonoidMap`]:
>
> ```diff
> - newtype SignedMultiset a = SMS {unSMS ::       Map a      Int }
> + newtype SignedMultiset a = SMS {unSMS :: MonoidMap a (Sum Int)}
> ```
>
> Here we've used the [`Sum`] wrapper type, whose [`Monoid`] instance defines [`mempty`][`Monoid.mempty`] as `Sum 0`, and [`<>`] as ordinary addition.
>
> Now we can redefine [`insertMany`][`SignedMultiSet.insertMany`] (and similar operations) in a simpler way:
>
> ```patch
>   insertMany :: Ord a => a -> Int -> SignedMultiset a -> SignedMultiset a
> + insertMany x n = SMS . MonoidMap.adjust (+ Sum n) x . unSMS
> - insertMany x n = SMS . Map.alter f x . unSMS
> -   where
> -     f Nothing  = Just n
> -     f (Just m) = let k = m + n in if k == 0 then Nothing else Just k
> ```
>
> Since the [`MonoidMap.adjust`] operation performs automatic minimisation, values of `Sum 0` are automatically excluded from the internal data structure, and there is no need to handle them differently from non-zero values.

### Example: `SetMultiMap` (a set-based multimap type)

> The [`multi-containers`] library provides the [`SetMultiMap`] type, which is internally defined as a [`Map`] from keys to (possibly-empty) sets of values, together with a `Size` parameter that records the total number of elements in the map (counting duplicates):
>
> ```hs
> newtype SetMultimap k a = SetMultimap (Map k (Set a), Size)
> type Size = Int
> ```
>
> All [`SetMultiMap`] operations maintain an invariant that the internal [`Map`] **must not** contain any mappings to empty sets. This requires [`SetMultiMap`] functions to detect and eliminate values of [`Set.empty`] (indicated by the [`Set.null`] function).
>
> For example, the [`alterWithKey`][`SetMultiMap.alterWithKey`] operation detects if the updated set is empty, and if so, performs a deletion instead of an insertion:
>
> ```hs
> alterWithKey :: Ord k => (k -> Set a -> Set a) -> k -> SetMultimap k a -> SetMultimap k a
> alterWithKey f k mm@(SetMultimap (m, _))
>     | Set.null as = fromMap (Map.delete k    m)
>     | otherwise   = fromMap (Map.insert k as m)
>   where
>     as = f k (mm ! k)
>
> fromMap :: Map k (Set a) -> SetMultimap k a
> fromMap m = SetMultimap (m', sum (fmap Set.size m'))
>   where
>     m' = Map.filter (not . Set.null) m
> ```
>
> Let's redefine [`SetMultiMap`] in terms of [`MonoidMap`]:
>
> ```patch
> - newtype SetMultimap k a = SetMultimap (      Map k (Set a), Size)
> + newtype SetMultimap k a = SetMultimap (MonoidMap k (Set a), Size)
> ```
>
> Now we can provide a simpler definition for [`alterWithKey`][`SetMultiMap.alterWithKey`] (and other operations):
>
> ```hs
> alterWithKey :: Ord k => (k -> Set a -> Set a) -> k -> SetMultimap k a -> SetMultimap k a
> alterWithKey f k (SetMultimap (m, size)) = SetMultiMap
>     (MonoidMap.set k new m, size - Set.size old + Set.size new)
>   where
>     old = MonoidMap.get k m
>     new = f k old
> ```
>
> Since the [`MonoidMap.set`] operation performs automatic minimisation, empty sets are automatically excluded from the internal data structure, and there is no need to handle them any differently from non-empty sets.

### Example: `MultiMap` (a list-based multimap type)

> The [`multi-containers`] library provides the [`MultiMap`] type, which is internally defined as a [`Map`] from keys to non-empty lists of values, together with a `Size` parameter that records the total number of elements in the map (counting duplicates):
>
> ```hs
> newtype Multimap k a = Multimap (Map k (NonEmpty a), Size)
> type Size = Int
> ```
>
> All [`MultiMap`] operations maintain the invariant that the internal [`Map`] **must not** contain any mappings to empty lists. This invariant is handled rather nicely by the use of the [`NonEmpty`] list type, which disallows empty lists _by construction_. As a result, it's arguably more difficult to make a mistake in the implementation than it would be if [`MultiMap`] were defined in terms of ordinary lists.
>
> However, certain operations still need to differentiate between the empty and non-empty case, and it's still necessary to handle each case specially.
>
> For example, the [`alterWithKey`][`MultiMap.alterWithKey`] operation detects if the updated list is empty, and if so, performs a deletion instead of an insertion:
>
> ```hs
> alterWithKey :: Ord k => (k -> [a] -> [a]) -> k -> Multimap k a -> Multimap k a
> alterWithKey f k mm@(Multimap (m, _)) = case nonEmpty (f k (mm ! k)) of
>     Just as' -> fromMap (Map.insert k as' m)
>     Nothing  -> fromMap (Map.delete k     m)
>
> fromMap :: Map k (NonEmpty a) -> Multimap k a
> fromMap m = Multimap (m, sum (fmap length m))
> ```
>
> Let's redefine [`MultiMap`] in terms of [`MonoidMap`] and ordinary lists:
>
> ```patch
> - newtype Multimap k a = Multimap (      Map k (NonEmpty a), Size)
> + newtype Multimap k a = Multimap (MonoidMap k          [a], Size)
> ```
>
> Now we can provide a simpler definition for [`alterWithKey`][`MultiMap.alterWithKey`] (and other operations):
> ```hs
> alterWithKey :: Ord k => (k -> [a] -> [a]) -> k -> Multimap k a -> Multimap k a
> alterWithKey f k (Multimap (m, size)) = MultiMap
>     (MonoidMap.set k new m, size - List.length old + List.length new)
>   where
>     old = MonoidMap.get k m
>     new = f k old
> ```
>
> Since the [`MonoidMap.set`] operation performs automatic minimisation:
> - empty lists are automatically excluded from the internal data structure.
> - there is no need to use a specialised [`NonEmpty`] type.
> - there is no need to handle empty lists differently from non-empty lists.

### Example: `MultiAsset` (a nested map type)

> The [`cardano-ledger`] library provides the [`MultiAsset`] type, which models a **nested** mapping from [`PolicyID`][`MultiAsset.PolicyID`] keys to [`AssetName`][`MultiAsset.AssetName`] keys to [`Integer`] values:
>
> ```hs
> newtype MultiAsset c = MultiAsset (Map (PolicyID c) (Map AssetName Integer))
> ```
>
> Each [`Integer`] value represents the value of an **asset** on the Cardano blockchain, where each asset is uniquely identified by the combination of a [`PolicyID`][`MultiAsset.PolicyID`] and an [`AssetName`][`MultiAsset.AssetName`]. (Multiple assets can share the same [`PolicyID`][`MultiAsset.PolicyID`].)
>
> All [`MultiAsset`] operations maintain a **dual invariant** that:
> - there must be no mappings from [`PolicyID`][`MultiAsset.PolicyID`] keys to empty maps; and that
> - there must be no mappings from [`AssetName`][`MultiAsset.AssetName`] keys to [`Integer`] values of `0`.
>
> To satisfy this invariant, [`MultiAsset`] operations use a variety of helper functions to ensure that [`MultiAsset`] values are always in a canonical form.
>
> For example, consider the [`Semigroup`] instance for [`MultiAsset`]:
>
> ```hs
> instance Semigroup (MultiAsset c) where
>     MultiAsset m1 <> MultiAsset m2 =
>         MultiAsset (canonicalMapUnion (canonicalMapUnion (+)) m1 m2)
> ```
>
> The above definition of [`<>`] performs pointwise addition of all pairs of values for matching assets.
>
> For example, if:
> - [`MultiAsset`] `m1` maps asset `a` to a value of `10`;
> - [`MultiAsset`] `m2` maps asset `a` to a value of `20`;
>
> Then:
> - [`MultiAsset`] `m1 <> m2` will map asset `a` to a value of `30`.
>
> The definition of [`<>`] uses a function called [`canonicalMapUnion`][`CanonicalMaps.canonicalMapUnion`], which does the heavy lifting work of performing a union while ensuring that each resulting [`Map`] is in canonical form.
>
> Let's have a look at the definition of [`canonicalMapUnion`][`CanonicalMaps.canonicalMapUnion`]:
>
> ```hs
> canonicalMapUnion ::
>   (Ord k, CanonicalZero a) =>
>   (a -> a -> a) ->
>   Map k a ->
>   Map k a ->
>   Map k a
> canonicalMapUnion _f t1 Tip                 = t1
> canonicalMapUnion  f t1 (Bin _ k x Tip Tip) = canonicalInsert f k x t1
> canonicalMapUnion  f (Bin _ k x Tip Tip) t2 = canonicalInsert f k x t2
> canonicalMapUnion _f Tip t2                 = t2
> canonicalMapUnion  f (Bin _ k1 x1 l1 r1) t2 = case Map.splitLookup k1 t2 of
>   (l2, mb, r2) -> case mb of
>     Nothing ->
>       if x1 == zeroC
>         then link2 l1l2 r1r2
>         else link k1 x1 l1l2 r1r2
>     Just x2 ->
>       if new == zeroC
>         then link2 l1l2 r1r2
>         else link k1 new l1l2 r1r2
>       where
>         new = f x1 x2
>     where
>       !l1l2 = canonicalMapUnion f l1 l2
>       !r1r2 = canonicalMapUnion f r1 r2
> ```
>
> The [`canonicalMapUnion`][`CanonicalMaps.canonicalMapUnion`] function in turn relies on [`canonicalInsert`][`CanonicalMaps.canonicalInsert`], which handles individual insertions:
>
> ```hs
> canonicalInsert ::
>   (Ord k, CanonicalZero a) =>
>   (a -> a -> a) ->
>   k ->
>   a ->
>   Map k a ->
>   Map k a
> canonicalInsert f !kx x = go
>   where
>     go Tip = if x == zeroC then Tip else Map.singleton kx x
>     go (Bin sy ky y l r) =
>       case compare kx ky of
>         LT -> link ky y (go l) r
>         GT -> link ky y l (go r)
>         EQ -> if new == zeroC then link2 l r else Bin sy kx new l r
>           where
>             new = f y x
> ```
>
> Similarly, the [`insertMultiAsset`][`MultiAsset.insertMultiAsset`] function, which "inserts" the value of an individual asset into a [`MultiAsset`] value, has the following definition:
>
> ```hs
> insertMultiAsset ::
>   (Integer -> Integer -> Integer) ->
>   PolicyID c ->
>   AssetName ->
>   Integer ->
>   MultiAsset c ->
>   MultiAsset c
> insertMultiAsset combine pid aid new (MultiAsset m1) =
>   case Map.splitLookup pid m1 of
>     (l1, Just m2, l2) ->
>       case Map.splitLookup aid m2 of
>         (v1, Just old, v2) ->
>           if n == 0
>             then
>               let m3 = link2 v1 v2
>                in if Map.null m3
>                     then MultiAsset (link2 l1 l2)
>                     else MultiAsset (link pid m3 l1 l2)
>             else MultiAsset (link pid (link aid n v1 v2) l1 l2)
>           where
>             n = combine old new
>         (_, Nothing, _) ->
>           MultiAsset
>             ( link
>                 pid
>                 ( if new == 0
>                     then m2
>                     else Map.insert aid new m2
>                 )
>                 l1
>                 l2
>             )
>     (l1, Nothing, l2) ->
>       MultiAsset
>         ( if new == 0
>             then link2 l1 l2
>             else link pid (Map.singleton aid new) l1 l2
>         )
> ```
>
> A notable feature of all the above functions is that they completely eschew the use of [`Map.merge`]. Instead, they directly manipulate constructors exported from [`Map.Internal`]. This approach was probably taken for performance reasons.
>
> However, it's clear that maintaining the invariant in this way comes at a **cost**: the code is rather complex, and it were not for a comprehensive test suite, it would probably be very easy to introduce a regression.
>
> In the spirit of demonstration, let's see what happens if we redefine the [`MultiAsset`] type in terms of [`MonoidMap`]:
>
> ```patch
> - newtype MultiAsset c = MultiAsset (Map       (PolicyID c) (      Map AssetName      Integer))
> + newtype MultiAsset c = MultiAsset (MonoidMap (PolicyID c) (MonoidMap AssetName (Sum Integer))
> ```
>
> Note that we have replaced [`Integer`] with <code><a href="https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:Sum">Sum</a> <a href="https://hackage.haskell.org/package/base/docs/Prelude.html#t:Integer">Integer</a></code>, whose [`Monoid`] instance defines [`mempty`][`Monoid.mempty`] as <code><a href="https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:Sum">Sum</a> 0</code>, and whose [`Semigroup`] instance defines [`<>`] as equivalent to ordinary integer addition.
>
> Recall that all [`MonoidMap`] operations automatically take care of the invariant that values cannot be [`mempty`][`Monoid.mempty`]. For the [`MultiAsset`] type, this means that:
> - outer maps are now prevented from including any mappings from [`PolicyID`][`MultiAsset.PolicyID`] to empty inner maps.
> - inner maps are now prevented from including any mappings from [`AssetName`][`MultiAsset.AssetName`] to values of <code><a href="https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:Sum">Sum</a> 0</code>.
>
> As a result, we can remove virtually all code that deals with canonicalisation.
>
> For example, we can now simplify the [`Semigroup`] instance for [`MultiAsset`], dispensing entirely with the need to call [`canonicalMapUnion`][`CanonicalMaps.canonicalMapUnion`]:
>
> ```patch
>   instance Semigroup (MultiAsset c) where
>       MultiAsset m1 <> MultiAsset m2 =
> -         MultiAsset (canonicalMapUnion (canonicalMapUnion (+)) m1 m2)
> +         m1 <> m2
> ```
>
> Given that the above instance is trivial, we can even derive the [`Semigroup`] and [`Monoid`] instances automatically:
>
> ```patch
>   newtype MultiAsset c = MultiAsset (MonoidMap (PolicyID c) (MonoidMap AssetName (Sum Integer))
> +     deriving newtype (Semigroup, Monoid)
> ```
>
> We can also simplify the [`insertMultiAsset`][`MultiAsset.insertMultiAsset`] function:
>
> ```patch
>   insertMultiAsset ::
>     (Integer -> Integer -> Integer) ->
>     PolicyID c ->
>     AssetName ->
>     Integer ->
>     MultiAsset c ->
>     MultiAsset c
>   insertMultiAsset combine pid aid new (MultiAsset m1) =
> +   MultiAsset $
> +   MonoidMap.adjust
> +     (MonoidMap.adjust (\(M.Sum old) -> M.Sum (combine old new)) aid) pid m1
> -  ...
> -  ### 27 lines deleted ###
> -  ...
> ```
>
> Finally, since [`MonoidMap`] already provides [`Eq`] and [`Group`] instances that are defined in terms of the underlying monoidal value type, we can automatically derive [`Eq`] and [`Group`] instances for [`MultiAsset`]:
>
> ```patch
>   newtype MultiAsset c = MultiAsset (MonoidMap (PolicyID c) (MonoidMap AssetName (Sum Integer))
> -     deriving newtype (Semigroup, Monoid)
> +     deriving newtype (Eq, Semigroup, Monoid, Group)
>
> - instance Eq (MultiAsset c) where
> -   MultiAsset x == MultiAsset y = pointWise (pointWise (==)) x y
> -
> - instance Group (MultiAsset c) where
> -   invert (MultiAsset m) =
> -     MultiAsset (canonicalMap (canonicalMap ((-1 :: Integer) *)) m)
> ```
>
> Many other simplifications are also possible. (Left as an exercise for readers!)

# Comparison with other generalised map types

The Haskell ecosystem has several different types for maps with monoidal properties, and several different types that model total functions from keys to values. Each type comes with its own set of advantages and limitations.

Here's a comparison between the [`MonoidMap`] type provided by this library and types provided by other libraries:

<table>
<thead>
  <tr valign="top" align="left">
    <th rowspan="2">Type<br><br><br></th>
    <th colspan="2">Features</th>
    <th colspan="5">Class Instances</th>
  </tr>
  <tr valign="top" align="left">
    <th>
      Models<br/>total<br/>functions
    </th>
    <th>
      Performs<br/>automatic<br/>minimisation
    </th>
    <th>
      <a href="https://hackage.haskell.org/package/base/docs/Data-Eq.html#t:Eq">
        <code>Eq</code>
      </a>
    </th>
    <th>
      <a href="https://hackage.haskell.org/package/monoid-subclasses">
        <code>Monoid</code><br/><em>subclasses</em>
      </a>
    </th>
    <th>
      <a href="https://hackage.haskell.org/package/groups/docs/Data-Group.html#t:Group">
        <code>Group</code>
      </a>
    </th>
    <th>
      <a href="https://hackage.haskell.org/package/base/docs/Data-Functor.html#t:Functor">
        <code>Functor</code>
      </a>
    </th>
    <th>
      <a href="https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Applicative">
        <code>Applicative</code>
      </a>
    </th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>
      <a href="https://github.com/jonathanknowles/monoidmap">
        <code><em>monoidmap</em></code>
      </a>
      <br/>
      <a href="https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#t:MonoidMap">
        <code>MonoidMap</code>
      </a>
      <br/>
      (this library)
    </td>
    <td>:heavy_check_mark:</td>
    <td>:heavy_check_mark:</td>
    <td>:heavy_check_mark:</td>
    <td>:heavy_check_mark:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
    <td>:x:</td>
  </tr>
  <tr>
    <td>
      <a href="https://hackage.haskell.org/package/monoid-map">
        <code><em>monoid‑map</em></code>
      </a>
      <br/>
      <a href="https://hackage.haskell.org/package/monoid-map/docs/Data-MonoidMap.html">
        <code>MonoidMap</code>
      </a>
    </td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
  </tr>
  <tr>
    <td>
      <a href="https://hackage.haskell.org/package/monoidal-containers">
        <code><em>monoidal‑containers</em></code>
      </a>
      <br/>
      <a href="https://hackage.haskell.org/package/monoidal-containers/docs/Data-Map-Monoidal.html#t:MonoidalMap">
        <code>MonoidalMap</code>
      </a>
    </td>
    <td>:x:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
  </tr>
  <tr>
    <td>
      <a href="https://hackage.haskell.org/package/total-map">
        <code><em>total‑map</em></code>
      </a>
      <br/>
      <a href="https://hackage.haskell.org/package/total-map/docs/Data-TotalMap.html#t:TMap">
        <code>TMap</code>
      </a>
    </td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
    <td>:x:</td>
    <td>:x:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:heavy_check_mark:</td>
  </tr>
  <tr>
    <td>
      <a href="https://hackage.haskell.org/package/total-maps">
        <code><em>total‑maps</em></code>
      </a>
      <br/>
      <a href="https://hackage.haskell.org/package/total-maps/docs/Data-Total-Map-Sparse.html#t:TotalSparseMap">
        <code>TotalSparseMap</code>
      </a>
    </td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:heavy_check_mark:</td>
  </tr>
  <tr>
    <td>
      <a href="https://hackage.haskell.org/package/defaultable-map">
        <code><em>defaultable‑map</em></code>
      </a>
      <br/>
      <a href="https://hackage.haskell.org/package/defaultable-map/docs/Defaultable-Map.html#t:Defaultable">
        <code>DefaultableMap</code>
      </a>
    </td>
    <td>:x:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:heavy_check_mark:</td>
  </tr>
  <tr>
    <td>
      <a href="https://hackage.haskell.org/package/chatter">
        <code><em>chatter</em></code>
      </a>
      <br/>
      <a href="https://hackage.haskell.org/package/chatter/docs/Data-DefaultMap.html#t:DefaultMap">
        <code>DefaultMap</code>
      </a>
    </td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
  </tr>
  <tr>
    <td>
      <a href="https://hackage.haskell.org/package/stack">
        <code><em>stack</em></code>
      </a>
      <br/>
      <a href="https://hackage.haskell.org/package/stack/docs/Data-Monoid-Map.html#t:MonoidMap">
        <code>MonoidMap</code>
      </a>
    </td>
    <td>:x:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
    <td>:heavy_check_mark:</td>
    <td>:heavy_check_mark:</td>
    <td>:x:</td>
  </tr>
</tbody>
</table>

[`cardano-ledger`]: https://github.com/input-output-hk/cardano-ledger
[`containers`]: https://hackage.haskell.org/package/containers
[`groups`]: https://hackage.haskell.org/package/groups
[`monoid-subclasses`]: https://hackage.haskell.org/package/monoid-subclasses
[`multi-containers`]: https://hackage.haskell.org/package/multi-containers
[`signed-multiset`]: https://hackage.haskell.org/package/signed-multiset

[`<>`]: https://hackage.haskell.org/package/base/docs/Data-Semigroup.html#v:-60--62-
[`CanonicalMaps.canonicalInsert`]: https://github.com/input-output-hk/cardano-ledger/blob/b00e28698d9c7fbbeda1c9cfdd1238d3bc4569cf/libs/cardano-data/src/Data/CanonicalMaps.hs#L69
[`CanonicalMaps.canonicalMapUnion`]: https://github.com/input-output-hk/cardano-ledger/blob/b00e28698d9c7fbbeda1c9cfdd1238d3bc4569cf/libs/cardano-data/src/Data/CanonicalMaps.hs#L42
[`Eq`]: https://hackage.haskell.org/package/base/docs/Data-Eq.html#t:Eq
[`Functor`]: https://hackage.haskell.org/package/base/docs/Data-Functor.html#t:Functor
[`Group`]: https://hackage.haskell.org/package/groups/docs/Data-Group.html#t:Group
[`Integer`]: https://hackage.haskell.org/package/base/docs/Prelude.html#t:Integer
[`Map.Internal`]: https://hackage.haskell.org/package/containers/docs/Data-Map-Internal.html
[`Map.merge`]: https://hackage.haskell.org/package/containers/docs/Data-Map-Merge-Strict.html#v:merge
[`Map`]: https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html#t:Map
[`Maybe`]: https://hackage.haskell.org/package/base/docs/Data-Maybe.html#t:Maybe
[`Monoid.mempty`]: https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:mempty
[`MonoidMap.adjust`]: https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:adjust
[`MonoidMap.empty`]: https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:empty
[`MonoidMap.intersection`]: https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:intersection
[`MonoidMap.invert`]: https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:invert
[`MonoidMap.map`]: https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:map
[`MonoidMap.minus`]: https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:minus
[`MonoidMap.set`]: https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:set
[`MonoidMap.union`]: https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#v:union
[`MonoidMap`]: https://jonathanknowles.github.io/monoidmap/Data-MonoidMap.html#t:MonoidMap
[`MonoidNull.null`]: https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null
[`MonoidNull`]: https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#t:MonoidNull
[`Monoid`]: https://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Monoid
[`MultiAsset.AssetName`]: https://github.com/input-output-hk/cardano-ledger/blob/b00e28698d9c7fbbeda1c9cfdd1238d3bc4569cf/eras/mary/impl/src/Cardano/Ledger/Mary/Value.hs#L110
[`MultiAsset.PolicyID`]: https://github.com/input-output-hk/cardano-ledger/blob/b00e28698d9c7fbbeda1c9cfdd1238d3bc4569cf/eras/mary/impl/src/Cardano/Ledger/Mary/Value.hs#L140
[`MultiAsset.insertMultiAsset`]: https://github.com/input-output-hk/cardano-ledger/blob/b00e28698d9c7fbbeda1c9cfdd1238d3bc4569cf/eras/mary/impl/src/Cardano/Ledger/Mary/Value.hs#LL831C1-L868C10
[`MultiAsset`]: https://github.com/input-output-hk/cardano-ledger/blob/b00e28698d9c7fbbeda1c9cfdd1238d3bc4569cf/eras/mary/impl/src/Cardano/Ledger/Mary/Value.hs#L157
[`MultiMap.alterWithKey`]: https://hackage.haskell.org/package/multi-containers/docs/Data-Multimap.html#v:alterWithKey
[`MultiMap`]: https://hackage.haskell.org/package/multi-containers/docs/Data-Multimap.html#t:Multimap
[`NestedMonoidMap`]: https://github.com/jonathanknowles/monoidmap/blob/main/components/monoidmap-examples/Examples/NestedMonoidMap.hs
[`NonEmpty`]: https://hackage.haskell.org/package/base/docs/Data-List-NonEmpty.html#t:NonEmpty
[`Nothing`]: https://hackage.haskell.org/package/base/docs/Data-Maybe.html#v:Nothing
[`Semigroup`]: https://hackage.haskell.org/package/base/docs/Data-Semigroup.html#t:Semigroup
[`Set.empty`]: https://hackage.haskell.org/package/containers/docs/Data-Set.html#v:empty
[`Set.intersection`]: https://hackage.haskell.org/package/containers/docs/Data-Set.html#v:intersection
[`Set.null`]: https://hackage.haskell.org/package/containers/docs/Data-Set.html#v:null
[`Set.union`]: https://hackage.haskell.org/package/containers/docs/Data-Set.html#v:union
[`SetMultiMap.alterWithKey`]: https://hackage.haskell.org/package/multi-containers/docs/Data-Multimap-Set.html#v:alterWithKey
[`SetMultiMap`]: https://hackage.haskell.org/package/multi-containers/docs/Data-Multimap-Set.html#t:SetMultimap
[`Set`]: https://hackage.haskell.org/package/containers/docs/Data-Set.html#t:Set
[`SignedMultiSet.insertMany`]: https://hackage.haskell.org/package/signed-multiset/docs/Data-SignedMultiset.html#v:insertMany
[`SignedMultiSet`]: https://hackage.haskell.org/package/signed-multiset/docs/Data-SignedMultiset.html#t:SignedMultiset
[`String`]: https://hackage.haskell.org/package/base/docs/Data-String.html#t:String
[`Sum`]: https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:Sum
[`TMap.trim`]: https://hackage.haskell.org/package/total-map/docs/Data-TotalMap.html#v:trim
[`TMap`]: https://hackage.haskell.org/package/total-map/docs/Data-TotalMap.html#t:TMap
