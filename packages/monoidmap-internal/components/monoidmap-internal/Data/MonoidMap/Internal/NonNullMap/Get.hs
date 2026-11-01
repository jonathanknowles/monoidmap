{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @get@, which gets the value
-- associated with a key in a 'NonNullMap', returning 'mempty' if the key is
-- not present.
--
-- These implementations are exported so that they can be benchmarked against
-- one another.
--
module Data.MonoidMap.Internal.NonNullMap.Get
    ( get_findWithDefault
    , get_lookup
    )
    where

import Prelude

import Data.Maybe
    ( fromMaybe )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap
    , toMap
    )

import qualified Data.Map.Strict as Map

get_findWithDefault
    :: (Ord k, Monoid v) => k -> NonNullMap k v -> v
get_findWithDefault k m = Map.findWithDefault mempty k $ toMap m
{-# INLINABLE get_findWithDefault #-}

get_lookup
    :: (Ord k, Monoid v) => k -> NonNullMap k v -> v
get_lookup k m = fromMaybe mempty $ Map.lookup k $ toMap m
{-# INLINABLE get_lookup #-}
