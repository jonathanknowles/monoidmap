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
-- This module provides the 'NonNull' type of values that are not 'Null.null',
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
    , fromMapWith_mapMaybe
    , fromMapWith_map_filter
    , fromMapWith_map_filter_if_any_null
    )
    where

import Prelude

import Data.Map.Strict
    ( Map )
import Data.MonoidMap.Internal.NonNull.FromMap
    ( fromMap_filter
    , fromMap_filter_if_any_null
    )
import Data.MonoidMap.Internal.NonNull.Unsafe
    ( NonNull
    , NonNullMap (..)
    , applyNonNull
    , applyNonNull2
    , fromNonNullMap
    , maybeNonNull
    )

import qualified Data.Map.Strict as Map
import qualified Data.Monoid.Null as Null

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Constructs a map of non-null values from an ordinary 'Map', removing all
--   'Null.null' values.
--
-- This function selects which of the @fromMap_*@ implementations in
-- "Data.MonoidMap.Internal.NonNull.FromMap" is used by the public @fromMap@
-- function of the @monoidmap@ package.
--
fromMap :: Null.MonoidNull v => Map k v -> Map k (NonNull v)
fromMap = fromMap_filter_if_any_null
{-# INLINE fromMap #-}

-- | Constructs a map of non-null values from an ordinary 'Map', applying the
--   given function to all values, and removing all 'Null.null' values.
--
-- This function selects which of the @fromMapWith_*@ implementations below is
-- used by the public @fromMapWith@ function of the @monoidmap@ package.
--
fromMapWith
    :: Null.MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith = fromMapWith_mapMaybe
{-# INLINE fromMapWith #-}

fromMapWith_mapMaybe
    :: Null.MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith_mapMaybe f = Map.mapMaybe (maybeNonNull . f)
{-# INLINABLE fromMapWith_mapMaybe #-}

fromMapWith_map_filter
    :: Null.MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith_map_filter f = fromMap_filter . Map.map f
{-# INLINABLE fromMapWith_map_filter #-}

fromMapWith_map_filter_if_any_null
    :: Null.MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith_map_filter_if_any_null f = fromMap_filter_if_any_null . Map.map f
{-# INLINABLE fromMapWith_map_filter_if_any_null #-}
