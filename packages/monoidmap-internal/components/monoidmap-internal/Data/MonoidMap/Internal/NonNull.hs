-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides /internal/ support for the @monoidmap@ package.
--
-- This module provides the 'NonNull' type of values that are not 'Null.null',
-- and functions for constructing maps of 'NonNull' values from ordinary maps.
--
module Data.MonoidMap.Internal.NonNull
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
    )
    where

import Data.Map.Strict
    ( Map )
import Data.MonoidMap.Internal.NonNull.FromMap
    ( fromMap_filter_if_any_null )
import Data.MonoidMap.Internal.NonNull.FromMapWith
    ( fromMapWith_mapMaybe )
import Data.MonoidMap.Internal.NonNull.Unsafe
    ( NonNull
    , NonNullMap (..)
    , applyNonNull
    , applyNonNull2
    , fromNonNullMap
    , maybeNonNull
    )

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
-- This function selects which of the @fromMapWith_*@ implementations in
-- "Data.MonoidMap.Internal.NonNull.FromMapWith" is used by the public
-- @fromMapWith@ function of the @monoidmap@ package.
--
fromMapWith
    :: Null.MonoidNull v2 => (v1 -> v2) -> Map k v1 -> Map k (NonNull v2)
fromMapWith = fromMapWith_mapMaybe
{-# INLINE fromMapWith #-}
