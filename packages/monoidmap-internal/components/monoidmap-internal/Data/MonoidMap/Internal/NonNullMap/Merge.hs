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

newtype WhenOneSideNull f k vx vr = WhenOneSideNull
    (Map.WhenMissing f k (NonNull vx) (NonNull vr))

newtype WhenBothNonNull f k v1 v2 vr = WhenBothNonNull
    (Map.WhenMatched f k (NonNull v1) (NonNull v2) (NonNull vr))

data Strategy f k v1 v2 v3 = Strategy
    { withNonNullL :: !(WhenOneSideNull f k v1    v3)
    , withNonNullR :: !(WhenOneSideNull f k    v2 v3)
    , withNonNullP :: !(WhenBothNonNull f k v1 v2 v3)
    }

--------------------------------------------------------------------------------
-- Strategies for when one side is null
--------------------------------------------------------------------------------

keepNull
    :: Applicative f
    => WhenOneSideNull f k v1 v2
keepNull = WhenOneSideNull Map.dropMissing
{-# INLINE keepNull #-}

keepNonNull
    :: Applicative f
    => WhenOneSideNull f k v v
keepNonNull = WhenOneSideNull Map.preserveMissing
{-# INLINE keepNonNull #-}

withNonNull
    :: (Applicative f, MonoidNull v2)
    => (v1 -> v2)
    -> WhenOneSideNull f k v1 v2
withNonNull f
    = WhenOneSideNull
    $ Map.mapMaybeMissing
    $ \_k v -> NonNull.nonNull $ NonNull.apply f v
{-# INLINE withNonNull #-}

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

withBoth
    :: (Applicative f, MonoidNull v3)
    => (v1 -> v2 -> v3)
    -> WhenBothNonNull f k v1 v2 v3
withBoth f
    = WhenBothNonNull
    $ Map.zipWithMaybeMatched
    $ \_k v1 v2 -> NonNull.nonNull $ NonNull.apply2 f v1 v2
{-# INLINE withBoth #-}

withBothA
    :: (Applicative f, MonoidNull v3)
    => (v1 -> v2 -> f v3)
    -> WhenBothNonNull f k v1 v2 v3
withBothA f
    = WhenBothNonNull
    $ Map.zipWithMaybeAMatched
    $ \_k v1 v2 -> NonNull.nonNull <$> NonNull.apply2 f v1 v2
{-# INLINE withBothA #-}
