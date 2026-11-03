-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides functions for merging maps of 'NonNull' values.
--
-- Every strategy provided by this module produces only 'NonNull' values.
--
-- This module is intended to be imported qualified:
--
-- @
-- import Data.MonoidMap.Internal.NonNullMap.Merge qualified as Merge
-- @
--
module Data.MonoidMap.Internal.NonNullMap.Merge
    (
    -- * Merging
      merge
    , mergeA

    -- * Strategies
    , Strategy (..)
    , WhenOneSideNull
    , WhenBothNonNull

    -- * Strategies for when one side is null
    , keepNull
    , keepNonNull
    , withNonNull
    , withNonNullA

    -- * Strategies for when both sides are non-null
    , withBoth
    , withBothA
    )
    where

import Prelude

import Data.Functor.Identity
    ( Identity )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNull
    ( NonNull )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap (..) )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.MonoidMap.Internal.NonNull as NonNull

--------------------------------------------------------------------------------
-- Merging
--------------------------------------------------------------------------------

-- | Merges two maps with the given t'Strategy'.
--
-- For each key that has a non-null value in at least one of the maps, the
-- strategy determines the value associated with that key in the merged map.
--
-- Keys that have a null value in both maps will always have a null value in
-- the merged map.
--
merge
    :: Ord k
    => Strategy Identity k v1 v2 v3
    -> NonNullMap k v1
    -> NonNullMap k v2
    -> NonNullMap k v3
merge
    (Strategy
        (WhenOneSideNull nnl)
        (WhenOneSideNull nnr)
        (WhenBothNonNull nnp))
    (NonNullMap m1)
    (NonNullMap m2) =
        NonNullMap $ Map.merge nnl nnr nnp m1 m2
{-# INLINE merge #-}

-- | Like 'merge', but for strategies with 'Applicative' effects.
--
-- Effects are performed in increasing order of keys.
--
mergeA
    :: (Applicative f, Ord k)
    => Strategy f k v1 v2 v3
    -> NonNullMap k v1
    -> NonNullMap k v2
    -> f (NonNullMap k v3)
mergeA
    (Strategy
        (WhenOneSideNull nnl)
        (WhenOneSideNull nnr)
        (WhenBothNonNull nnp))
    (NonNullMap m1)
    (NonNullMap m2) =
        NonNullMap <$> Map.mergeA nnl nnr nnp m1 m2
{-# INLINE mergeA #-}

--------------------------------------------------------------------------------
-- Strategies
--------------------------------------------------------------------------------

-- | A strategy for keys that have a non-null value in one map, and a null
--   value in the other.
--
-- Computes a result value of type @vr@ from the non-null value of type @vx@.
--
newtype WhenOneSideNull f k vx vr = WhenOneSideNull
    (Map.WhenMissing f k (NonNull vx) (NonNull vr))

-- | A strategy for keys that have non-null values in both maps.
--
-- Computes a result value of type @vr@ from the non-null values of types @v1@
-- and @v2@.
--
newtype WhenBothNonNull f k v1 v2 vr = WhenBothNonNull
    (Map.WhenMatched f k (NonNull v1) (NonNull v2) (NonNull vr))

-- | A strategy for merging two maps.
--
-- A strategy determines how to compute each value in the merged map from the
-- values associated with the same key in the two maps being merged.
--
-- A strategy has one component for every case in which at least one of the
-- two values is non-null. There is no component for the case in which both
-- values are null: the merged value is then always null, and the merge never
-- needs to visit such keys.
--
-- Consequently, a binary operation __@f@__ can only be implemented by a
-- strategy if it satisfies:
--
-- @
-- f 'mempty' 'mempty' '==' 'mempty'
-- @
--
-- For example, the following strategy implements '(<>)':
--
-- @
-- v'Strategy'
--     { 'withNonNullL' = 'keepNonNull'      -- v '<>' 'mempty' '==' v
--     , 'withNonNullR' = 'keepNonNull'      -- 'mempty' '<>' v '==' v
--     , 'withNonNullP' = 'withBoth' '(<>)'
--     }
-- @
--
data Strategy f k v1 v2 v3 = Strategy
    { withNonNullL :: !(WhenOneSideNull f k v1    v3)
      -- ^ Applies to keys with a non-null value in the first map only.
    , withNonNullR :: !(WhenOneSideNull f k    v2 v3)
      -- ^ Applies to keys with a non-null value in the second map only.
    , withNonNullP :: !(WhenBothNonNull f k v1 v2 v3)
      -- ^ Applies to keys with non-null values in both maps.
    }

--------------------------------------------------------------------------------
-- Strategies for when one side is null
--------------------------------------------------------------------------------

-- | Produces a null value, discarding the non-null value.
--
-- Equivalent to @'withNonNull' ('const' 'mempty')@, but more efficient, as it
-- discards entire subtrees without visiting their values.
--
keepNull
    :: Applicative f
    => WhenOneSideNull f k v1 v2
keepNull = WhenOneSideNull Map.dropMissing
{-# INLINE keepNull #-}

-- | Produces the non-null value unchanged.
--
-- Equivalent to @'withNonNull' 'id'@, but more efficient, as it reuses entire
-- subtrees without testing whether their values are null.
--
keepNonNull
    :: Applicative f
    => WhenOneSideNull f k v v
keepNonNull = WhenOneSideNull Map.preserveMissing
{-# INLINE keepNonNull #-}

-- | Applies the given function to the non-null value.
--
withNonNull
    :: (Applicative f, MonoidNull v2)
    => (v1 -> v2)
    -> WhenOneSideNull f k v1 v2
withNonNull f
    = WhenOneSideNull
    $ Map.mapMaybeMissing
    $ \_k v -> NonNull.nonNull $ NonNull.apply f v
{-# INLINE withNonNull #-}

-- | Applies the given effectful function to the non-null value.
--
withNonNullA
    :: (Applicative f, MonoidNull v2)
    => (v1 -> f v2)
    -> WhenOneSideNull f k v1 v2
withNonNullA f
    = WhenOneSideNull
    $ Map.traverseMaybeMissing
    $ \_k v -> NonNull.nonNull <$> NonNull.apply f v
{-# INLINE withNonNullA #-}

--------------------------------------------------------------------------------
-- Strategies for when both sides are non-null
--------------------------------------------------------------------------------

-- | Applies the given function to both non-null values.
--
withBoth
    :: (Applicative f, MonoidNull v3)
    => (v1 -> v2 -> v3)
    -> WhenBothNonNull f k v1 v2 v3
withBoth f
    = WhenBothNonNull
    $ Map.zipWithMaybeMatched
    $ \_k v1 v2 -> NonNull.nonNull $ NonNull.apply2 f v1 v2
{-# INLINE withBoth #-}

-- | Applies the given effectful function to both non-null values.
--
withBothA
    :: (Applicative f, MonoidNull v3)
    => (v1 -> v2 -> f v3)
    -> WhenBothNonNull f k v1 v2 v3
withBothA f
    = WhenBothNonNull
    $ Map.zipWithMaybeAMatched
    $ \_k v1 v2 -> NonNull.nonNull <$> NonNull.apply2 f v1 v2
{-# INLINE withBothA #-}
