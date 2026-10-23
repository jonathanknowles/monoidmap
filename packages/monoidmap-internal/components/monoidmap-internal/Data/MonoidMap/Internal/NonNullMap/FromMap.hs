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
module Data.MonoidMap.Internal.NonNullMap.FromMap
    ( fromMap_filter
    , fromMap_filter_if_any_null
    , fromMap_mapMaybe
    )
    where

import Prelude hiding
    ( null )

import Data.Map.Strict
    ( Map )
import Data.Monoid.Null
    ( MonoidNull (null) )
import Data.MonoidMap.Internal.NonNull
    ( NonNull, nonNull )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap (..)
    , unsafeFromMap
    )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

fromMap_filter :: MonoidNull v => Map k v -> NonNullMap k v
fromMap_filter = unsafeFromMap . Map.filter (\(!v) -> not (null v))
{-# INLINABLE fromMap_filter #-}

fromMap_filter_if_any_null :: MonoidNull v => Map k v -> NonNullMap k v
fromMap_filter_if_any_null m
    | F.any (\(!v) -> null v) m =
        unsafeFromMap $ Map.filter (\(!v) -> not (null v)) m
    | otherwise =
        unsafeFromMap m
{-# INLINABLE fromMap_filter_if_any_null #-}

fromMap_mapMaybe :: MonoidNull v => Map k v -> NonNullMap k v
fromMap_mapMaybe = NonNullMap . Map.mapMaybe nonNull
{-# INLINABLE fromMap_mapMaybe #-}
