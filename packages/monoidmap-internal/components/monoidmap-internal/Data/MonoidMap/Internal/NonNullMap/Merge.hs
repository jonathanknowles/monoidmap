-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides strategies for merging maps of 'NonNull' values.
--
-- Every strategy provided by this module produces only 'NonNull' values.
--
module Data.MonoidMap.Internal.NonNullMap.Merge
    ( WhenOneSideNull
    , WhenBothNonNull
    , MergeStrategy (..)
    , keepNull
    , keepNonNull
    , withNonNull
    , withNonNullA
    , withBoth
    , withBothA
    )
    where

import Prelude

import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNull
    ( NonNull )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.MonoidMap.Internal.NonNull as NonNull

type WhenOneSideNull f k          vx                        vr
   = Map.WhenMissing f k (NonNull vx)              (NonNull vr)
type WhenBothNonNull f k          v1           v2           vr
   = Map.WhenMatched f k (NonNull v1) (NonNull v2) (NonNull vr)

data MergeStrategy f k v1 v2 v3 = MergeStrategy
    { withNonNullL :: !(WhenOneSideNull f k v1    v3)
    , withNonNullR :: !(WhenOneSideNull f k    v2 v3)
    , withNonNullP :: !(WhenBothNonNull f k v1 v2 v3)
    }

keepNull
    :: Applicative f
    => WhenOneSideNull f k v1 v2
keepNull = Map.dropMissing
{-# INLINE keepNull #-}

keepNonNull
    :: Applicative f
    => WhenOneSideNull f k v v
keepNonNull = Map.preserveMissing
{-# INLINE keepNonNull #-}

withNonNull
    :: (Applicative f, MonoidNull v2)
    => (v1 -> v2)
    -> WhenOneSideNull f k v1 v2
withNonNull f
    = Map.mapMaybeMissing
    $ \_k v -> NonNull.nonNull $ NonNull.apply f v
{-# INLINE withNonNull #-}

withNonNullA
    :: (Applicative f, MonoidNull v2)
    => (v1 -> f v2)
    -> WhenOneSideNull f k v1 v2
withNonNullA f
    = Map.traverseMaybeMissing
    $ \_k v -> NonNull.nonNull <$> NonNull.apply f v
{-# INLINE withNonNullA #-}

withBoth
    :: (Applicative f, MonoidNull v3)
    => (v1 -> v2 -> v3)
    -> WhenBothNonNull f k v1 v2 v3
withBoth f
    = Map.zipWithMaybeMatched
    $ \_k v1 v2 -> NonNull.nonNull $ NonNull.apply2 f v1 v2
{-# INLINE withBoth #-}

withBothA
    :: (Applicative f, MonoidNull v3)
    => (v1 -> v2 -> f v3)
    -> WhenBothNonNull f k v1 v2 v3
withBothA f
    = Map.zipWithMaybeAMatched
    $ \_k v1 v2 -> NonNull.nonNull <$> NonNull.apply2 f v1 v2
{-# INLINE withBothA #-}
