{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides /internal/ support for the @monoidmap@ package.
--
-- This module provides the 'NonNull' type of values that are not 'null',
-- and functions for constructing maps of 'NonNull' values from ordinary maps.
--
module Data.MonoidMap.Internal
    (
    -- * Types
      NonNull
    , NonNullMap (..)

    -- * Non-null values
    , maybeNonNull
    , applyNonNull
    , applyNonNull2

    -- * Non-null maps
    , fromNonNullMap

    -- * Construction
    , fromMap
    , fromMapWith

    -- ** Implementations
    , fromMap_filter
    , fromMap_filter_if_any_null
    , fromMap_mapMaybe
    , fromMapWith_mapMaybe
    , fromMapWith_map_filter
    , fromMapWith_map_filter_if_any_null
    )
    where

import Prelude hiding
    ( null )

import Data.Map.Strict
    ( Map )
import Data.Monoid.Null
    ( MonoidNull (null) )
import Data.MonoidMap.Internal.NonNull.Unsafe
    ( NonNull
    , maybeNonNull
    , unwrap
    )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap (..)
    , fromNonNullMap
    , unsafeToNonNullMap
    )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Non-null values
--------------------------------------------------------------------------------

applyNonNull :: (v -> a) -> (NonNull v -> a)
applyNonNull f = f . unwrap
{-# INLINE applyNonNull #-}

applyNonNull2 :: (v1 -> v2 -> a) -> (NonNull v1 -> NonNull v2 -> a)
applyNonNull2 f v1 v2 = f (unwrap v1) (unwrap v2)
{-# INLINE applyNonNull2 #-}

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Constructs a map of non-null values from an ordinary 'Map', removing all
--   'null' values.
--
-- This function selects which of the @fromMap_*@ implementations below is used
-- by the public @fromMap@ function of the @monoidmap@ package.
--
fromMap :: MonoidNull v => Map k v -> Map k (NonNull v)
fromMap = fromMap_filter_if_any_null
{-# INLINE fromMap #-}

fromMap_filter :: MonoidNull v => Map k v -> Map k (NonNull v)
fromMap_filter = unsafeToNonNullMap . Map.filter (\(!v) -> not (null v))
{-# INLINABLE fromMap_filter #-}

fromMap_filter_if_any_null :: MonoidNull v => Map k v -> Map k (NonNull v)
fromMap_filter_if_any_null m
    | F.any (\(!v) -> null v) m =
        unsafeToNonNullMap $ Map.filter (\(!v) -> not (null v)) m
    | otherwise =
        unsafeToNonNullMap m
{-# INLINABLE fromMap_filter_if_any_null #-}

fromMap_mapMaybe :: MonoidNull v => Map k v -> Map k (NonNull v)
fromMap_mapMaybe = Map.mapMaybe maybeNonNull
{-# INLINABLE fromMap_mapMaybe #-}

-- | Constructs a map of non-null values from an ordinary 'Map', applying the
--   given function to all values, and removing all 'null' values.
--
-- This function selects which of the @fromMapWith_*@ implementations below is
-- used by the public @fromMapWith@ function of the @monoidmap@ package.
--
fromMapWith
    :: MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith = fromMapWith_mapMaybe
{-# INLINE fromMapWith #-}

fromMapWith_mapMaybe
    :: MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith_mapMaybe f = Map.mapMaybe (maybeNonNull . f)
{-# INLINABLE fromMapWith_mapMaybe #-}

fromMapWith_map_filter
    :: MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith_map_filter f = fromMap_filter . Map.map f
{-# INLINABLE fromMapWith_map_filter #-}

fromMapWith_map_filter_if_any_null
    :: MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith_map_filter_if_any_null f = fromMap_filter_if_any_null . Map.map f
{-# INLINABLE fromMapWith_map_filter_if_any_null #-}
