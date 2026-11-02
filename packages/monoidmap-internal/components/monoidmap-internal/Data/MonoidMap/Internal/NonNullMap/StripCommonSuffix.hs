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
    )
    where

import Prelude

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
