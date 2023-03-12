# `total-monoidal-maps`

<a href="http://jonathanknowles.net/total-monoidal-maps/"><img src="https://img.shields.io/badge/API-Documentation-green" /></a>

## Overview

This library provides the [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type, with the following features:

- Models a [total relation](#totality) from unique keys to monoidal values.
- Performs [automatic canonicalisation](#automatic-canonicalisation) of [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html) values.
- Provides instances of type classes defined in the [`monoid-subclasses`](https://hackage.haskell.org/package/monoid-subclasses) library.
- Provides instances of type classes defined in the [`groups`](https://hackage.haskell.org/package/groups/docs/Data-Group.html) library.

See the following sections for comparisons of the [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type with other types:
-  [Comparison with standard map types](#comparison-with-standard-map-types)
-  [Comparison with other map types](#comparison-with-other-map-types)

## Totality

Conceptually, the [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type models a **total relation** from keys to monoidal values.

Every possible value of type `k` is associated with a corresponding value of type `v`:
```hs
MonoidMap.get :: (Ord k, Monoid v) => k -> MonoidMap k v -> v
```

By default, every key in an [`empty`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:empty) map is associated with a value of [`mempty`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:mempty):
```hs
∀ k. MonoidMap.get k MonoidMap.empty == mempty
```

## Comparison with standard map types

The [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type differs from the standard [containers](https://hackage.haskell.org/package/containers) [`Map`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html#t:Map) type in how it relates _keys_ to _values_:

| Type            | Relation                                                 |
|----------------:|:---------------------------------------------------------|
|       `Map k v` | relates _keys_ of type `k` to _values_ of type `Maybe v` |
| `MonoidMap k v` | relates _keys_ of type `k` to _values_ of type `v`       |

This becomes evident if we compare the type signatures of operations to query a key for its value, for both types:

```hs
      Map.lookup ::             k ->       Map k v -> Maybe v
MonoidMap.get    :: Monoid v => k -> MonoidMap k v ->       v
```

For _unconstrained_ value types, using [`Maybe`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Maybe) makes it possible to signal the _presence_ or _absence_ of a value for a particular key. However, _monoidal_ types have a natural way to represent empty values: the [`mempty`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:mempty) constant, which represents the _null_ or _identity_ element of a [`Monoid`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Monoid).

Consequently, using a standard [`Map`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html#t:Map) with a _monoidal_ value type gives rise to _two_ distinct representations for null or empty values:

| `Map.lookup k m` | Interpretation                                                      |
|:-----------------|:--------------------------------------------------------------------|
| `Nothing`        | Map `m` has _no_ entry for key `k`.                                 |
| `Just mempty`    | Map `m` _does_ have an entry for key `k`, but the value is _empty_. |

In constrast, the [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type provides a single, _canonical_ representation for null values, according to the following mapping:

| `Map.lookup k m`        | ⟼ | `MonoidMap.get k m`     |
|:------------------------|---|:------------------------|
| `Nothing`               | ⟼ | `mempty`               |
| `Just v \| v == mempty` | ⟼ | `mempty`               |
| `Just v \| v /= mempty` | ⟼ | `v`                    |

## Automatic canonicalisation
 
Internally, the [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type uses a sparse [`Map`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html#t:Map) data structure to store its key-value mappings, and _only_ stores mappings for values that are not [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null), where [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) is an indicator function on monoidal values that satisfies the following property:

```hs
null :: (Eq v, MonoidNull v) => v -> Bool
null v == (v == mempty)
```

Every function that produces a [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) performs automatic canonicalisation, so that [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values are never included in the result. Consequently, the internal data structure is always in canonical form.

### Advantages of automatic canonicalisation

Automatic canonicalisation of [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values makes it possible to perform certain operations in _constant time_, rather than in linear time, as it is never necessary to traverse the entire map in order to determine which values may or may not be [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null):

<table>
<thead>
  <tr>
    <th rowspan="2">Operation</th>
    <th colspan="2">With<br>Canonicalisation</th>
    <th colspan="2">Without<br>Canonicalisation</th>
  </tr>
  <tr>
    <th>Lower<br>Bound</th>
    <th>Upper<br>Bound</th>
    <th>Lower<br>Bound</th>
    <th>Upper<br>Bound<br></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td><a href="http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:null" rel="nofollow"><code>null</code></a></td>
    <td>$\Omega(1)$</td>
    <td>$O(1)$</td>
    <td>$\Omega(n)$</td>
    <td>$O(n)$</td>
  </tr>
  <tr>
    <td><a href="http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:nonNull" rel="nofollow"><code>nonNull</code></a></td>
    <td>$\Omega(1)$</td>
    <td>$O(1)$</td>
    <td>$\Omega(n)$</td>
    <td>$O(n)$</td>
  </tr>
  <tr>
    <td><a href="http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:nonNullCount" rel="nofollow"><code>nonNullCount</code></a></td>
    <td>$\Omega(1)$</td>
    <td>$O(1)$</td>
    <td>$\Omega(n)$</td>
    <td>$O(n)$</td>
  </tr>
  <tr>
    <td><a href="http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:toMap" rel="nofollow"><code>toMap</code></a></td>
    <td>$\Omega(1)$</td>
    <td>$O(1)$</td>
    <td>$\Omega(n)$</td>
    <td>$O(n)$</td>
  </tr>
  <tr>
    <td><a href="https://hackage.haskell.org/package/base/docs/Data-Eq.html#t:Eq" rel="nofollow"><code>(==)</code></a></td>
    <td>$\Omega(1)$</td>
    <td>$O(n)$</td>
    <td>$\Omega(n)$</td>
    <td>$O(n)$</td>
  </tr>
</tbody>
</table>

### Limitations of automatic canonicalisation

The [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type (currently) has no [`Functor`](https://hackage.haskell.org/package/base/docs/Data-Functor.html#t:Functor) instance, as the requirement to perform canonicalisation of  [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values means that [`map`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:map) must remove [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values from its result. Therefore, the [`map`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:map) operation does _not_ unconditionally satisfy the functor composition law:

```hs
map (g . f) == map g . map f
```

However, the composition law _can_ be satisfied _conditionally_, provided that function `f` always maps non-[`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values in the source [`Monoid`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Monoid) to non-[`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values in the target [`Monoid`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Monoid):

```hs
(∀ v. (v /= mempty) ==> (f v /= mempty))
    ==>
    map (g . f) == map g . map f
```

But if function `f` maps non-[`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values in the source [`Monoid`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Monoid) to [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values in the target [`Monoid`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Monoid), this can give rise to a violation of the functor composition law, as [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values are never included in the result of applying `map f`.

<details><summary><strong>Example violation</strong></summary>
<br/>

Consider the following [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) `m`:
```hs
m :: MonoidMap String String
m = singleton "k" "v"
```

And the following functions `f` and `g`:
```hs
f :: (Monoid a, Monoid b) => a -> b
f = const mempty

g :: Monoid b => String -> String
g = const "z"
```

By substituting the above definitions into the left-hand side of the functor composition law, we obtain:
```hs
map (g . f) m = map (const "z" . const mempty) (singleton "k" "v")
              = map (const "z"               ) (singleton "k" "v")
              =                                (singleton "k" "z")
```

By substituting the above definitions into the right-hand side of the functor composition law, we obtain:
```hs
map g (map f m) = map (const "z") (map (const mempty) (singleton "k" "v"))
                = map (const "z") mempty
                =                 mempty
```

This leads to the following inequality between the left-hand side and right-hand side:
```hs
singleton "k" "z" /= mempty
```
Therefore, for this example, the functor composition law is not satisfied.

</details>

## Comparison with other map types

[Hackage](https://hackage.haskell.org/) has several different types for maps with monoidal properties, and several different types that model total relations from keys to values. Each type comes with its own set of advantages and limitations.

Here's a comparison between the [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type provided by this library and types provided by other libraries:

| Type<br><br><br> | Total<br>key‑value<br>relation<br> | Canonical<br>default<br>values<br> | [`Eq`<br>class](https://hackage.haskell.org/package/base/docs/Data-Eq.html#t:Eq)<br>instance<br> | [`Monoid`<br>subclass](https://hackage.haskell.org/package/monoid-subclasses)<br>instances<br> | [`Group`<br>class](https://hackage.haskell.org/package/groups/docs/Data-Group.html#t:Group)<br>instance<br> | [`Functor` <br>class](https://hackage.haskell.org/package/base/docs/Data-Functor.html#t:Functor)<br>instance<br> | [`Applicative` <br>class](https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Applicative)<br>instance<br> |
|:--|:--|:--|:--|:--|:--|:--|:--|
| [_`total-monoidal-maps`_](https://github.com/jonathanknowles/total-monoidal-maps)<br>[**`MonoidMap`**](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) (this library) | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :x: | :x: |
| [_`monoid-map`_](https://hackage.haskell.org/package/monoid-map)<br>[**`MonoidMap`**](https://hackage.haskell.org/package/monoid-map/docs/Data-MonoidMap.html) | :x: | :heavy_check_mark: | :heavy_check_mark: | :x: | :heavy_check_mark: | :heavy_check_mark: | :x: |
| [_`monoidal-containers`_](https://hackage.haskell.org/package/monoidal-containers)<br>[**`MonoidalMap`**](https://hackage.haskell.org/package/monoidal-containers/docs/Data-Map-Monoidal.html#t:MonoidalMap) | :x: | :x: | :heavy_check_mark: | :x: | :x: | :heavy_check_mark: | :x: |
| [_`total-map`_](https://hackage.haskell.org/package/total-map)<br>[**`TMap`**](https://hackage.haskell.org/package/total-map/docs/Data-TotalMap.html#t:TMap) | :heavy_check_mark: | :x: | :x: | :x: | :x: | :heavy_check_mark: | :heavy_check_mark: |
| [_`total-maps`_](https://hackage.haskell.org/package/total-maps)<br>[**`TotalSparseMap`**](https://hackage.haskell.org/package/total-maps/docs/Data-Total-Map-Sparse.html#t:TotalSparseMap) | :heavy_check_mark: | :x: | :heavy_check_mark: | :x: | :x: | :heavy_check_mark: | :heavy_check_mark: |
| [_`defaultable-map`_](https://hackage.haskell.org/package/defaultable-map)<br>[**`Defaultable`**](https://hackage.haskell.org/package/defaultable-map-1.0.2/docs/Defaultable-Map.html#t:Defaultable) | :x: | :x: | :heavy_check_mark: | :x: | :x: | :heavy_check_mark: | :heavy_check_mark: |
| [_`chatter`_](https://hackage.haskell.org/package/chatter)<br>[**`DefaultMap`**](https://hackage.haskell.org/package/chatter-0.9.1.0/docs/Data-DefaultMap.html#t:DefaultMap) | :heavy_check_mark: | :x: | :heavy_check_mark: | :x: | :x: | :heavy_check_mark: | :x: |
| [_`stack`_](https://hackage.haskell.org/package/stack)<br>[**`MonoidMap`**](https://hackage.haskell.org/package/stack/docs/Data-Monoid-Map.html#t:MonoidMap) | :x: | :x: | :heavy_check_mark: | :x: | :heavy_check_mark: | :heavy_check_mark: | :x: |
