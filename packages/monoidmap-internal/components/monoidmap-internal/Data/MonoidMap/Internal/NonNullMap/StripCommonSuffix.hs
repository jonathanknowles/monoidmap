{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @stripCommonSuffix@, which strips the
-- greatest common suffix from a pair of maps of 'NonNull' values.
--
-- These implementations are exported so that they can be benchmarked against
-- one another.
--
module Data.MonoidMap.Internal.NonNullMap.StripCommonSuffix
    ( stripCommonSuffix_default
    , stripCommonSuffix_triples
    , stripCommonSuffix_triples_matched
    , stripCommonSuffix_lists
    , stripCommonSuffix_merge3
    )
    where

import Prelude

import Data.Map.Internal
    ( Map (Bin, Tip), link, link2 )
import Data.Maybe
    ( fromJust )
import Data.Monoid.GCD
    ( RightGCDMonoid )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.Merge
    ( Strategy (withNonNullL, withNonNullR, withNonNullP) )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap (..) )

import qualified Data.Map.Strict as Map
import qualified Data.Monoid.GCD as C
import qualified Data.MonoidMap.Internal.NonNull as NonNull
import qualified Data.MonoidMap.Internal.NonNullMap.CommonSuffix as NonNullMap
import qualified Data.MonoidMap.Internal.NonNullMap.Map as NonNullMap
import qualified Data.MonoidMap.Internal.NonNullMap.Merge as Merge
import qualified Data.MonoidMap.Internal.NonNullMap.StripSuffix as NonNullMap

-- | Follows the default implementation of 'C.stripCommonSuffix' provided by the
--   @monoid-subclasses@ package.
--
-- This implementation first computes the greatest common suffix @s@, and
-- then strips @s@ from each map. As @s@ is a suffix of both maps, stripping
-- @s@ from either map cannot fail. But this implementation discards the
-- evidence of this, and so must use 'fromJust' to recover the results.
--
stripCommonSuffix_default
    :: (Ord k, MonoidNull v, RightGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> (NonNullMap k v, NonNullMap k v, NonNullMap k v)
stripCommonSuffix_default m1 m2 =
    ( fromJust (NonNullMap.stripSuffix s m1)
    , fromJust (NonNullMap.stripSuffix s m2)
    , s
    )
  where
    s = NonNullMap.commonSuffix m1 m2
{-# INLINABLE stripCommonSuffix_default #-}

-- | Merges both maps into a single map of triples, and then splits this map
--   into three.
--
-- For each key, this implementation computes the greatest common suffix and
-- both remainders together, with a single call to 'C.stripCommonSuffix'.
--
stripCommonSuffix_triples
    :: (Ord k, MonoidNull v, RightGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> (NonNullMap k v, NonNullMap k v, NonNullMap k v)
stripCommonSuffix_triples m1 m2 =
    ( NonNullMap.map remainder1 triples
    , NonNullMap.map remainder2 triples
    , NonNullMap.map suffix triples
    )
  where
    triples = Merge.merge Merge.Strategy
        { withNonNullL =
            Merge.withNonNull (\v -> (v, mempty, mempty))
            -- Justification:
            --
            -- stripCommonSuffix a mempty ≡ (a, mempty, mempty)

        , withNonNullR =
            Merge.withNonNull (\v -> (mempty, v, mempty))
            -- Justification:
            --
            -- stripCommonSuffix mempty a ≡ (mempty, a, mempty)

        , withNonNullP =
            Merge.withBoth C.stripCommonSuffix
        }
        m1 m2
    remainder1 (r1, _, _) = r1
    remainder2 (_, r2, _) = r2
    suffix (_, _, s) = s
{-# INLINABLE stripCommonSuffix_triples #-}

-- | Merges both maps into a map of triples for only those keys present in
--   both maps, and then uses this map to construct the results.
--
-- Unlike 'stripCommonSuffix_triples', this implementation does not
-- construct a triple for any key present in only one map, as the results
-- for such a key are already known: the greatest common suffix is
-- 'mempty', and the remainder is the value itself. This allows the
-- remainders to share subtrees with the original maps.
--
stripCommonSuffix_triples_matched
    :: (Ord k, MonoidNull v, RightGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> (NonNullMap k v, NonNullMap k v, NonNullMap k v)
stripCommonSuffix_triples_matched m1 m2 =
    ( Merge.merge remainder1 m1 triples
    , Merge.merge remainder2 m2 triples
    , NonNullMap.map suffix triples
    )
  where
    triples = Merge.merge Merge.Strategy
        { withNonNullL =
            Merge.keepNull
        , withNonNullR =
            Merge.keepNull
        , withNonNullP =
            Merge.withBoth C.stripCommonSuffix
        }
        m1 m2
    remainder1 = Merge.Strategy
        { withNonNullL =
            Merge.keepNonNull
            -- Justification:
            --
            -- stripCommonSuffix a mempty ≡ (a, mempty, mempty)

        , withNonNullR =
            Merge.keepNull
            -- Justification:
            --
            -- Every key of 'triples' is also a key of 'm1'.

        , withNonNullP =
            Merge.withBoth (\_ (r1, _, _) -> r1)
        }
    remainder2 = Merge.Strategy
        { withNonNullL =
            Merge.keepNonNull
            -- Justification:
            --
            -- stripCommonSuffix mempty a ≡ (mempty, a, mempty)

        , withNonNullR =
            Merge.keepNull
            -- Justification:
            --
            -- Every key of 'triples' is also a key of 'm2'.

        , withNonNullP =
            Merge.withBoth (\_ (_, r2, _) -> r2)
        }
    suffix (_, _, s) = s
{-# INLINABLE stripCommonSuffix_triples_matched #-}

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
stripCommonSuffix_lists
    :: (Ord k, MonoidNull v, RightGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> (NonNullMap k v, NonNullMap k v, NonNullMap k v)
stripCommonSuffix_lists (NonNullMap m1) (NonNullMap m2) =
    go [] [] [] (Map.toAscList m1) (Map.toAscList m2)
  where
    -- The accumulators are forced at every step, so that they do not build up
    -- chains of thunks.
    go !r1s !r2s !ss [] [] =
        (fromDescList r1s, fromDescList r2s, fromDescList ss)
    go r1s r2s ss (x : xs) [] =
        go (x : r1s) r2s ss xs []
    go r1s r2s ss [] (y : ys) =
        go r1s (y : r2s) ss [] ys
    go r1s r2s ss xxs@(x@(k1, v1) : xs) yys@(y@(k2, v2) : ys) =
        case compare k1 k2 of
            LT ->
                go (x : r1s) r2s ss xs yys
            GT ->
                go r1s (y : r2s) ss xxs ys
            EQ ->
                go
                    (consNonNull k1 r1 r1s)
                    (consNonNull k1 r2 r2s)
                    (consNonNull k1 s ss)
                    xs
                    ys
              where
                (r1, r2, s) = NonNull.apply2 C.stripCommonSuffix v1 v2

    consNonNull k v kvs = case NonNull.nonNull v of
        Nothing -> kvs
        Just nv -> (k, nv) : kvs

    fromDescList = NonNullMap . Map.fromDistinctDescList
{-# INLINABLE stripCommonSuffix_lists #-}

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
stripCommonSuffix_merge3
    :: (Ord k, MonoidNull v, RightGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> (NonNullMap k v, NonNullMap k v, NonNullMap k v)
stripCommonSuffix_merge3 (NonNullMap m1) (NonNullMap m2) =
    case go m1 m2 of
        (a, b, c) -> (NonNullMap a, NonNullMap b, NonNullMap c)
  where
    -- Whenever no key within a pair of subtrees has a non-null common suffix,
    -- the subtrees are returned unchanged, as:
    --
    -- commonSuffix a b ≡ mempty ==> stripCommonSuffix a b ≡ (a, b, mempty)
    --
    -- This allows the remainders to share subtrees with the original maps.
    --
    go Tip t2 = (Tip, t2, Tip)
    go t1 Tip = (t1, Tip, Tip)
    go t1@(Bin _ k v1 lt1 rt1) t2 =
        let !(lt2, mv2, rt2) = Map.splitLookup k t2
            !(r1l, r2l, sl) = go lt1 lt2
            !(r1r, r2r, sr) = go rt1 rt2
        in
        case mv2 of
            Nothing
                | Map.null sl && Map.null sr ->
                    (t1, t2, Tip)
                | otherwise ->
                    triple
                        (link k v1 r1l r1r)
                        (link2 r2l r2r)
                        (link2 sl sr)
            Just v2
                | Nothing <- ms, Map.null sl && Map.null sr ->
                    (t1, t2, Tip)
                | otherwise ->
                    triple
                        (linkNonNull k r1 r1l r1r)
                        (linkNonNull k r2 r2l r2r)
                        (linkMaybe k ms sl sr)
              where
                (r1, r2, s) = NonNull.apply2 C.stripCommonSuffix v1 v2
                ms = NonNull.nonNull s

    linkNonNull k v = linkMaybe k (NonNull.nonNull v)

    linkMaybe k mv l r = case mv of
        Nothing -> link2 l r
        Just !nv -> link k nv l r

    triple !a !b !c = (a, b, c)
{-# INLINABLE stripCommonSuffix_merge3 #-}
