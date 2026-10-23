{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @fromMapWith@, which constructs a
-- map of 'NonNull' values from an ordinary map, applying a function to all
-- values.
--
-- These implementations are exported so that they can be benchmarked against
-- one another.
--
module Data.MonoidMap.Internal.NonNullMap.FromMapWith
    ( fromMapWith_mapMaybe
    , fromMapWith_map_filter
    , fromMapWith_map_filter_if_any_null
    )
    where

import Prelude

import Data.Map.Strict
    ( Map )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNull
    ( NonNull, nonNull )
import Data.MonoidMap.Internal.NonNullMap.FromMap
    ( fromMap_filter
    , fromMap_filter_if_any_null
    )

import qualified Data.Map.Strict as Map

fromMapWith_mapMaybe
    :: MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith_mapMaybe f = Map.mapMaybe (nonNull . f)
{-# INLINABLE fromMapWith_mapMaybe #-}

fromMapWith_map_filter
    :: MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith_map_filter f = fromMap_filter . Map.map f
{-# INLINABLE fromMapWith_map_filter #-}

fromMapWith_map_filter_if_any_null
    :: MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith_map_filter_if_any_null f = fromMap_filter_if_any_null . Map.map f
{-# INLINABLE fromMapWith_map_filter_if_any_null #-}
