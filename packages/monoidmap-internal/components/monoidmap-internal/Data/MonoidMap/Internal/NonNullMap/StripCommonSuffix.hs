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
    ( NonNullMap )

import qualified Data.Monoid.GCD as C
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
