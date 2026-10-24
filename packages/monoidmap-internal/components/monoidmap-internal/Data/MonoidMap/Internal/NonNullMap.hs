-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'NonNullMap' type and related functions.
--
-- This module is intended to be imported qualified:
--
-- @
-- import Data.MonoidMap.Internal.NonNullMap (NonNullMap (..))
-- import Data.MonoidMap.Internal.NonNullMap qualified as NonNullMap
-- @
--
module Data.MonoidMap.Internal.NonNullMap
    (
    -- * Type
      NonNullMap (..)

    -- * Construction
    , fromMap
    , fromMapWith

    -- * Deconstruction
    , toMap
    )
    where

import Prelude hiding
    ( null )

import Data.Map.Strict
    ( Map )
import Data.Monoid.Null
    ( MonoidNull (null) )
import Data.MonoidMap.Internal.NonNull
    ( NonNull )
import Data.MonoidMap.Internal.NonNullMap.FromMap
    ( fromMap_filter_if_any_null )
import Data.MonoidMap.Internal.NonNullMap.FromMapWith
    ( fromMapWith_mapMaybe )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap (..), toMap )

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Constructs a 'NonNullMap' from an ordinary 'Map', removing all
--   'null' values.
--
-- This function selects which of the implementations in
-- "Data.MonoidMap.Internal.NonNullMap.FromMap" is used by the public @fromMap@
-- function of the @monoidmap@ package.
--
fromMap :: MonoidNull v => Map k v -> NonNullMap k v
fromMap = fromMap_filter_if_any_null
{-# INLINE fromMap #-}

-- | Constructs a 'NonNullMap' from an ordinary 'Map', applying the given
--   function to all values, and removing all 'null' values.
--
-- This function selects which of the implementations in
-- "Data.MonoidMap.Internal.NonNullMap.FromMapWith" is used by the public
-- @fromMapWith@ function of the @monoidmap@ package.
--
fromMapWith :: MonoidNull v2 => (v1 -> v2) -> Map k v1 -> NonNullMap k v2
fromMapWith = fromMapWith_mapMaybe
{-# INLINE fromMapWith #-}
