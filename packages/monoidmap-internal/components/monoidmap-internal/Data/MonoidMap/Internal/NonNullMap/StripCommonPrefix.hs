{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @stripCommonPrefix@, which strips the
-- greatest common prefix from a pair of maps of 'NonNull' values.
--
-- These implementations are exported so that they can be benchmarked against
-- one another.
--
module Data.MonoidMap.Internal.NonNullMap.StripCommonPrefix
    ( stripCommonPrefix_default
    , stripCommonPrefix_triples
    , stripCommonPrefix_triples_matched
    , stripCommonPrefix_lists
    , stripCommonPrefix_merge3
    )
    where

import Prelude

import Data.Map.Internal
    ( Map (Bin, Tip), link, link2 )
import Data.Maybe
    ( fromJust )
import Data.Monoid.GCD
    ( LeftGCDMonoid )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.Merge
    ( Strategy (withNonNullL, withNonNullR, withNonNullP) )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap (..) )

import qualified Data.Map.Strict as Map
import qualified Data.Monoid.GCD as C
import qualified Data.MonoidMap.Internal.NonNull as NonNull
import qualified Data.MonoidMap.Internal.NonNullMap.CommonPrefix as NonNullMap
import qualified Data.MonoidMap.Internal.NonNullMap.Map as NonNullMap
import qualified Data.MonoidMap.Internal.NonNullMap.Merge as Merge
import qualified Data.MonoidMap.Internal.NonNullMap.StripPrefix as NonNullMap

-- | Follows the default implementation of 'C.stripCommonPrefix' provided by the
--   @monoid-subclasses@ package.
--
-- This implementation first computes the greatest common prefix @p@, and
-- then strips @p@ from each map. As @p@ is a prefix of both maps, stripping
-- @p@ from either map cannot fail. But this implementation discards the
-- evidence of this, and so must use 'fromJust' to recover the results.
--
stripCommonPrefix_default
    :: (Ord k, MonoidNull v, LeftGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> (NonNullMap k v, NonNullMap k v, NonNullMap k v)
stripCommonPrefix_default m1 m2 =
    ( p
    , fromJust (NonNullMap.stripPrefix p m1)
    , fromJust (NonNullMap.stripPrefix p m2)
    )
  where
    p = NonNullMap.commonPrefix m1 m2
{-# INLINABLE stripCommonPrefix_default #-}

-- | Merges both maps into a single map of triples, and then splits this map
--   into three.
--
-- For each key, this implementation computes the greatest common prefix and
-- both remainders together, with a single call to 'C.stripCommonPrefix'.
--
stripCommonPrefix_triples
    :: (Ord k, MonoidNull v, LeftGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> (NonNullMap k v, NonNullMap k v, NonNullMap k v)
stripCommonPrefix_triples m1 m2 =
    ( NonNullMap.map prefix triples
    , NonNullMap.map remainder1 triples
    , NonNullMap.map remainder2 triples
    )
  where
    triples = Merge.merge Merge.Strategy
        { withNonNullL =
            Merge.withNonNull (\v -> (mempty, v, mempty))
            -- Justification:
            --
            -- stripCommonPrefix a mempty ≡ (mempty, a, mempty)

        , withNonNullR =
            Merge.withNonNull (\v -> (mempty, mempty, v))
            -- Justification:
            --
            -- stripCommonPrefix mempty a ≡ (mempty, mempty, a)

        , withNonNullP =
            Merge.withBoth C.stripCommonPrefix
        }
        m1 m2
    prefix (p, _, _) = p
    remainder1 (_, r1, _) = r1
    remainder2 (_, _, r2) = r2
{-# INLINABLE stripCommonPrefix_triples #-}

-- | Merges both maps into a map of triples for only those keys present in
--   both maps, and then uses this map to construct the results.
--
-- Unlike 'stripCommonPrefix_triples', this implementation does not
-- construct a triple for any key present in only one map, as the results
-- for such a key are already known: the greatest common prefix is
-- 'mempty', and the remainder is the value itself. This allows the
-- remainders to share subtrees with the original maps.
--
stripCommonPrefix_triples_matched
    :: (Ord k, MonoidNull v, LeftGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> (NonNullMap k v, NonNullMap k v, NonNullMap k v)
stripCommonPrefix_triples_matched m1 m2 =
    ( NonNullMap.map prefix triples
    , Merge.merge remainder1 m1 triples
    , Merge.merge remainder2 m2 triples
    )
  where
    triples = Merge.merge Merge.Strategy
        { withNonNullL =
            Merge.keepNull
        , withNonNullR =
            Merge.keepNull
        , withNonNullP =
            Merge.withBoth C.stripCommonPrefix
        }
        m1 m2
    remainder1 = Merge.Strategy
        { withNonNullL =
            Merge.keepNonNull
            -- Justification:
            --
            -- stripCommonPrefix a mempty ≡ (mempty, a, mempty)

        , withNonNullR =
            Merge.keepNull
            -- Justification:
            --
            -- Every key of 'triples' is also a key of 'm1'.

        , withNonNullP =
            Merge.withBoth (\_ (_, r1, _) -> r1)
        }
    remainder2 = Merge.Strategy
        { withNonNullL =
            Merge.keepNonNull
            -- Justification:
            --
            -- stripCommonPrefix mempty a ≡ (mempty, mempty, a)

        , withNonNullR =
            Merge.keepNull
            -- Justification:
            --
            -- Every key of 'triples' is also a key of 'm2'.

        , withNonNullP =
            Merge.withBoth (\_ (_, _, r2) -> r2)
        }
    prefix (p, _, _) = p
{-# INLINABLE stripCommonPrefix_triples_matched #-}

-- | Walks through the keys of both maps in ascending order, accumulating the
--   results for each key in lists, from which it then constructs the results.
--
-- Unlike the merge-based implementations, this implementation constructs no
-- intermediate map. However, it must visit every key of both maps, and it
-- cannot reuse subtrees of the original maps for keys present in only one
-- map.
--
-- As each list is accumulated in descending order of key, it is converted to
-- a map with 'Map.fromDistinctDescList'.
--
stripCommonPrefix_lists
    :: (Ord k, MonoidNull v, LeftGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> (NonNullMap k v, NonNullMap k v, NonNullMap k v)
stripCommonPrefix_lists (NonNullMap m1) (NonNullMap m2) =
    go [] [] [] (Map.toAscList m1) (Map.toAscList m2)
  where
    -- The accumulators are forced at every step, so that they do not build up
    -- chains of thunks.
    go !ps !r1s !r2s [] [] =
        (fromDescList ps, fromDescList r1s, fromDescList r2s)
    go ps r1s r2s (x : xs) [] =
        go ps (x : r1s) r2s xs []
    go ps r1s r2s [] (y : ys) =
        go ps r1s (y : r2s) [] ys
    go ps r1s r2s xxs@(x@(k1, v1) : xs) yys@(y@(k2, v2) : ys) =
        case compare k1 k2 of
            LT ->
                go ps (x : r1s) r2s xs yys
            GT ->
                go ps r1s (y : r2s) xxs ys
            EQ ->
                go
                    (consNonNull k1 p ps)
                    (consNonNull k1 r1 r1s)
                    (consNonNull k1 r2 r2s)
                    xs
                    ys
              where
                (p, r1, r2) = NonNull.apply2 C.stripCommonPrefix v1 v2

    consNonNull k v kvs = case NonNull.nonNull v of
        Nothing -> kvs
        Just nv -> (k, nv) : kvs

    fromDescList = NonNullMap . Map.fromDistinctDescList
{-# INLINABLE stripCommonPrefix_lists #-}

-- | Traverses both maps together, constructing all three results at once, in
--   the same way that the functions of "Data.Map.Merge.Strict" construct a
--   single result.
--
-- This implementation constructs no intermediate structures, and like the
-- merge-based implementations, it reuses subtrees of the original maps for
-- keys present in only one map. However, it depends on the internals of the
-- @containers@ package, to which the Package Versioning Policy does not
-- apply.
--
stripCommonPrefix_merge3
    :: (Ord k, MonoidNull v, LeftGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> (NonNullMap k v, NonNullMap k v, NonNullMap k v)
stripCommonPrefix_merge3 (NonNullMap m1) (NonNullMap m2) =
    case go m1 m2 of
        (a, b, c) -> (NonNullMap a, NonNullMap b, NonNullMap c)
  where
    -- Whenever no key within a pair of subtrees has a non-null common prefix,
    -- the subtrees are returned unchanged, as:
    --
    -- commonPrefix a b ≡ mempty ==> stripCommonPrefix a b ≡ (mempty, a, b)
    --
    -- This allows the remainders to share subtrees with the original maps.
    --
    go Tip t2 = (Tip, Tip, t2)
    go t1 Tip = (Tip, t1, Tip)
    go t1@(Bin _ k v1 lt1 rt1) t2 =
        let !(lt2, mv2, rt2) = Map.splitLookup k t2
            !(pl, r1l, r2l) = go lt1 lt2
            !(pr, r1r, r2r) = go rt1 rt2
        in
        case mv2 of
            Nothing
                | Map.null pl && Map.null pr ->
                    (Tip, t1, t2)
                | otherwise ->
                    triple
                        (link2 pl pr)
                        (link k v1 r1l r1r)
                        (link2 r2l r2r)
            Just v2
                | Nothing <- mp, Map.null pl && Map.null pr ->
                    (Tip, t1, t2)
                | otherwise ->
                    triple
                        (linkMaybe k mp pl pr)
                        (linkNonNull k r1 r1l r1r)
                        (linkNonNull k r2 r2l r2r)
              where
                (p, r1, r2) = NonNull.apply2 C.stripCommonPrefix v1 v2
                mp = NonNull.nonNull p

    linkNonNull k v = linkMaybe k (NonNull.nonNull v)

    linkMaybe k mv l r = case mv of
        Nothing -> link2 l r
        Just !nv -> link k nv l r

    triple !a !b !c = (a, b, c)
{-# INLINABLE stripCommonPrefix_merge3 #-}
