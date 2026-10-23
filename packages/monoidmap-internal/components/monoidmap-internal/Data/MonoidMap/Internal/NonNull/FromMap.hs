{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @fromMap@, which constructs a
-- map of 'NonNull' values from an ordinary map.
--
-- These implementations are exported so that they can be benchmarked against
-- one another.
--
module Data.MonoidMap.Internal.NonNull.FromMap
    ( fromMap_filter
    , fromMap_filter_if_any_null
    , fromMap_mapMaybe
    )
    where

import Prelude

import Data.Map.Strict
    ( Map )
import Data.MonoidMap.Internal.NonNull.Unsafe
    ( NonNull
    , maybeNonNull
    , unsafeToNonNullMap
    )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Monoid.Null as Null

fromMap_filter :: Null.MonoidNull v => Map k v -> Map k (NonNull v)
fromMap_filter = unsafeToNonNullMap . Map.filter (\(!v) -> not (Null.null v))
{-# INLINABLE fromMap_filter #-}

fromMap_filter_if_any_null :: Null.MonoidNull v => Map k v -> Map k (NonNull v)
fromMap_filter_if_any_null m
    | F.any (\(!v) -> Null.null v) m =
        unsafeToNonNullMap $ Map.filter (\(!v) -> not (Null.null v)) m
    | otherwise =
        unsafeToNonNullMap m
{-# INLINABLE fromMap_filter_if_any_null #-}

fromMap_mapMaybe :: Null.MonoidNull v => Map k v -> Map k (NonNull v)
fromMap_mapMaybe = Map.mapMaybe maybeNonNull
{-# INLINABLE fromMap_mapMaybe #-}
