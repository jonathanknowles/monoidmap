# `total-monoidal-maps`

<a href="http://jonathanknowles.net/total-monoidal-maps/"><img src="https://img.shields.io/badge/API-Documentation-green" /></a>

## Overview

This library provides the [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type, with the following features:

- Models a [total relation](#totality) from unique keys to monoidal values.
- Performs [automatic canonicalisation](#automatic-canonicalisation) of [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html) values.
- Provides instances of type classes defined in the [`monoid-subclasses`](https://hackage.haskell.org/package/monoid-subclasses) library.
- Provides instances of type classes defined in the [`groups`](https://hackage.haskell.org/package/groups/docs/Data-Group.html) library.

For comparisons of the [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type with other types, see:
-  [Comparison with standard map types](#comparison-with-standard-map-types)
-  [Comparison with other map types](#comparison-with-other-map-types)

For potential applications of this data type, see:
- [Applications of this data type](#applications-of-this-data-type)

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

## Applications of this data type

The [`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap) type is useful for building other types that:

  - model a total relation from keys to monoidal values; and
  - require canonicalisation of [`null`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html#v:null) monoidal values.

### Example

Consider the following type and operations:
```hs
newtype Index k v = Index {getIndex :: Map k (Set v)}
    deriving newtype Eq

-- | Retrieves the set of values associated with a key.
indexLookup :: (Ord k, Ord v) => k -> Index k v -> Set v

-- | Updates the set of values associated with a key.
indexUpdate :: (Ord k, Ord v) => k -> Set v -> Index k v -> Index k v

-- | Computes the intersection of two indices.
indexIntersection :: (Ord k, Ord v) => Index k v -> Index k v -> Index k v
```

The above type derives [`Eq`](https://hackage.haskell.org/package/base/docs/Data-Eq.html#t:Eq) from [`Map`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html#t:Map), which provides a fast lower bound on the time complexity of equality checks. But for the [`Eq`](https://hackage.haskell.org/package/base/docs/Data-Eq.html#t:Eq) instance to be correct, operations on `Index` must preserve an **_invariant_**: the internal [`Map`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html#t:Map) must **_never_** include keys that map to [`Set.empty`](https://hackage.haskell.org/package/containers-0.6.7/docs/Data-Set.html#v:empty).

There are two obvious solutions for enforcing this invariant:

#### Solution 1: Manually enforce the invariant

The author might choose to define the above operations with code similar to:

```hs
indexLookup :: (Ord k, Ord v) => k -> Index k v -> Set v
indexLookup k (Index i) = Map.findWithDefault Set.empty k i

indexUpdate :: (Ord k, Ord v) => k -> Set v -> Index k v -> Index k v
indexUpdate k vs (Index i)
    | Set.null vs = Index (Map.delete k    i)
    | otherwise   = Index (Map.insert k vs i)

indexIntersection :: (Ord k, Ord v) => Index k v -> Index k v -> Index k v
indexIntersection (Index i1) (Index i2) = Index $
    Map.merge
        Map.dropMissing
        Map.dropMissing
        (Map.zipWithMaybeMatched (const setIntersectionMaybe))
        i1
        i2
  where
    -- Return 'Nothing' if the resultant set is empty:
    setIntersectionMaybe :: Ord v => Set v -> Set v -> Maybe (Set v)
    setIntersectionMaybe s1 s2
        | Set.null s3 = Nothing
        | otherwise   = Just s3
      where
        s3 = Set.intersection s1 s2
```

While there's nothing inherently wrong with this solution, it does require some discipline on the part of the author, in order to avoid accidentally introducing [`Set.empty`](https://hackage.haskell.org/package/containers/docs/Data-Set.html#v:empty) into the internal map. To reduce the chance of this happening, a careful author might choose to write a suite of property tests to check the invariant is never violated. However, if other people (less careful than the original author) amend the code (perhaps to introduce a new operation), it's very easy to accidentally violate this invariant.

#### Solution 2: Redefine `Index` in terms of a non-empty set type

Let's assume there's a `NonEmptySet` type available, with the following operations:
```hs
-- | Constructs a non-empty set from an ordinary set. 
fromSet :: Set a -> Maybe (NonEmptySet a)

-- | Converts a non-empty set to an ordinary set.
toSet :: NonEmptySet a -> Set a

-- | Computes the intersection of two non-empty sets.
intersection :: Ord a => NonEmptySet a -> NonEmptySet a -> Maybe (NonEmptySet a)
```

Then the author could re-define `Index` in terms of the `NonEmptySet` type:
 
```hs
newtype Index k v = Index {getIndex :: Map k (NonEmptySet v)}
   deriving newtype Eq

indexLookup :: (Ord k, Ord v) => k -> Index k v -> Set v
indexLookup k (Index i) =
    maybe mempty NonEmptySet.toSet $ Map.lookup k i

indexUpdate :: (Ord k, Ord v) => k -> Set v -> Index k v -> Index k v
indexUpdate k vs (Index i) = case NonEmptySet.fromSet vs of
    Nothing -> Index (Map.delete k    i)
    Just zs -> Index (Map.insert k zs i)

indexIntersection :: (Ord k, Ord v) => Index k v -> Index k v -> Index k v
indexIntersection (Index i1) (Index i2) = Index $
    Map.merge
        Map.dropMissing
        Map.dropMissing
        (Map.zipWithMaybeMatched (const NonEmptySet.intersection))
        i1
        i2
```

This solution is a little shorter, and certainly makes it harder to introduce bugs, but it depends on the existence of a `NonEmptySet` type. If the monoidal value type of choice does not have a `NonEmpty` variant available, then the author might have to write one themselves.

This brings us to the last solution:

#### Solution 3: Redefine `Index` in terms of `MonoidMap`

By redefining the above type in terms of [`MonoidMap`]([`MonoidMap`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#t:MonoidMap)), we can greatly simplify the implementation to:
```hs
newtype Index k v = Index {getIndex :: MonoidMap k (Set v)}
    deriving newtype Eq

indexLookup :: (Ord k, Ord v) => k -> Index k v -> Set v
indexLookup k (Index i) = MonoidMap.get k i

indexUpdate :: (Ord k, Ord v) => k -> Set v -> Index k v -> Index k v
indexUpdate k vs (Index i) = Index $ MonoidMap.set k vs i

indexIntersection :: (Ord k, Ord v) => Index k v -> Index k v -> Index k v
indexIntersection (Index i1) (Index i2) = Index $
    MonoidMap.intersectionWith Set.intersection i1 i2
```

With this implementation, the empty set is automatically excluded from the internal data structure, so there's no longer any risk of violating the invariant.

We can also make a further simplification:
```hs
indexIntersection :: (Ord k, Ord v) => Index k v -> Index k v -> Index k v
indexIntersection (Index i1) (Index i2) = Index $ MonoidMap.gcd i1 i2
```

This takes advantage of the [`MonoidMap.gcd`](http://jonathanknowles.net/total-monoidal-maps/Data-Total-MonoidMap.html#v:gcd) operation, which in the case of the [`Set`](https://hackage.haskell.org/package/containers/docs/Data-Set.html#t:Set) type, computes the intersection of sets for matching pairs of keys.

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
