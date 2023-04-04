# `total-monoidal-maps`

<a href="http://jonathanknowles.net/total-monoidal-maps/"><img src="https://img.shields.io/badge/API-Documentation-green" /></a>

## Overview

This library provides the [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type, with the following features:

- Models a [total relation](#introduction) from unique keys to monoidal values.
- Performs [automatic canonicalisation](#automatic-canonicalisation) of [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html) values (values that are equal to [`mempty`](https://hackage.haskell.org/package/base/docs/Data-Monoid.html#v:mempty)).
- Provides instances of type classes defined in the [`monoid-subclasses`](https://hackage.haskell.org/package/monoid-subclasses) library.
- Provides instances of type classes defined in the [`groups`](https://hackage.haskell.org/package/groups/docs/Data-Group.html) library.

For comparisons of the [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type with other types, see:
-  [Comparison with standard map types](#comparison-with-standard-map-types)
-  [Comparison with other map types](#comparison-with-other-map-types)

## Introduction

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

All [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) operations perform automatic canonicalisation, so that [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values are never included within the internal data structure.

## Advantages of automatic canonicalisation

### Consistency

Automatic canonicalisation of [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values can help to ensure consistency when encoding to or decoding from other formats such as JSON, CBOR, or YAML.

For example, you may wish to ensure that:

- When _encoding_ a map, no [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values appear in the encoded result.
- When _decoding_ a map, no [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values appear in the decoded result.

### Correctness

Automatic canonicalisation of [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values can help to ensure correctness of operations that compare or combine maps. With the standard [containers](https://hackage.haskell.org/package/containers) [`Map`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html#t:Map) type, it's often necessary to consider cases such as:

- when key `k` is _present_ in map `m1`, but _absent_ from map `m2`.
- when key `k` is _present_ in map `m2`, but _absent_ from map `m1`.

Mishandling cases such as these can give rise to subtle bugs that manifest in unexpected places. For maps with value types that are more complex (such as maps that nest other maps), the number of cases requiring consideration can easily multiply, making it even easier to introduce bugs.

Since all [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) operations perform automatic canonicalisation, it's possible to write functions that compare or combine maps without having to consider `Nothing` and `Just mempty` as separate cases.

### Simplicity

Some total map data types adopt the approach of only performing canonicalisation when _explicitly demanded to_. For example, the [`TMap`](https://hackage.haskell.org/package/total-map/docs/Data-TotalMap.html#t:TMap) data type provides a [`trim`](https://hackage.haskell.org/package/total-map/docs/Data-TotalMap.html#v:trim) operation that traverses the map and removes any values that are equal to the _default_ value. This approach has some advantages, such the ability to provide a lawful [`Functor`](https://hackage.haskell.org/package/base/docs/Data-Functor.html#t:Functor) instance.

However, suppose that:
- Your application has a large, long-lived map data structure.
- You want your map data structure to have a minimal memory footprint.
- You want to avoid accumulating map entries with duplicate default values.

In principle, it seems easy to meet these requirements with a data structure like `TMap`. We can ensure there are no default values in the map simply by calling `trim` at certain points. However:

- Calling `trim` when it _isn't_ necessary might adversely affect performance, since `trim` must traverse the entire data structure.
- It might not be obvious _when_ it's necessary to call `trim`. For example, consider the following operation: `m1 <> m2`. Could this operation produce a map where some keys map to default values? (Answer: it depends on the choice of default value and the underlying value type.)
- It's rather easy to introduce a regression by removing an "unnecessary" call to `trim` that was actually necessary. (The compiler will not help here, as trimmed and untrimmed maps share the same type.)
- Although `trim` is marked as a semantic no-op, default values are _still_ made visible by operations that transform `TMap` values to values of other types.
  
  For example, you _might_ expect the following property to hold:
  ```hs
  show m == show (TMap.trim m)
  ```
  However, this property does not hold in general. Here's a counterexample:
  ```hs
  >>> m = TMap.singleton 'k' 'v' 'v'
  >>> show m
  TMap 'v' (fromList [('k', 'v')]) 
  >>> show (TMap.trim m)
  TMap 'v' (fromList [])
  ```

Since all [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) operations perform automatic canonicalisation, and take care to only perform canonicalisation _when necessary_, it's simpler to reason about the behaviour of client code.

### Compactness

Automatic canonicalisation of [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) ensures that space usage is minimised, as the internal data structure guarantees to only store key-value mappings for values that are not [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null). This can be important for long-lived data structures that are subject to repeated updates over time.

### Performance

Automatic canonicalisation of [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) values makes it possible to perform certain operations in _constant time_, rather than in linear time, as it is never necessary to traverse the entire map in order to determine which values may or may not be [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null):

<table>
<thead>
  <tr>
    <th>Operation</th>
    <th>With<br>Canonicalisation</th>
    <th>Without<br>Canonicalisation</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td><a href="http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:null" rel="nofollow"><code>null</code></a></td>
    <td>$O(1)$</td>
    <td>$O(n)$</td>
  </tr>
  <tr>
    <td><a href="http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:nonNull" rel="nofollow"><code>nonNull</code></a></td>
    <td>$O(1)$</td>
    <td>$O(n)$</td>
  </tr>
  <tr>
    <td><a href="http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:nonNullCount" rel="nofollow"><code>nonNullCount</code></a></td>
    <td>$O(1)$</td>
    <td>$O(n)$</td>
  </tr>
  <tr>
    <td><a href="http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:toMap" rel="nofollow"><code>toMap</code></a></td>
    <td>$O(1)$</td>
    <td>$O(n)$</td>
  </tr>
</tbody>
</table>

## Limitations of automatic canonicalisation

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

<details><summary><strong>Example violation of functor composition law</strong></summary>
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

<table>
<thead>
  <tr valign="top" align="left">
    <th rowspan="2">Type<br><br><br></th>
    <th colspan="2">Features</th>
    <th colspan="5">Class Instances</th>
  </tr>
  <tr valign="top" align="left">
    <th>
      Total<br/>key‑value<br/>relation
    </th>
    <th>
      Canonical<br/>default<br/>values
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
      <a href="https://github.com/jonathanknowles/total-monoidal-maps">
        <code><em>total‑monoidal‑maps</em></code>
      </a>
      <br/>
      <a href="http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap">
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
