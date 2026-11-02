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
    )
    where

import Prelude

import Data.Maybe
    ( fromJust )
import Data.Monoid.GCD
    ( LeftGCDMonoid )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.Merge
    ( Strategy (withNonNullL, withNonNullR, withNonNullP) )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap )

import qualified Data.Monoid.GCD as C
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
