{-# LANGUAGE RoleAnnotations #-}
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
-- This module defines the 'NonNull' type of values that are not 'C.null',
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
    , unsafeToNonNullMap

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

import Prelude

import Control.DeepSeq
    ( NFData )
import Data.Bifoldable
    ( Bifoldable )
import Data.Coerce
    ( coerce )
import Data.Functor.Classes
    ( Eq1, Eq2, Show1, Show2 )
import Data.Map.Strict
    ( Map )
import Data.Monoid.Null
    ( MonoidNull )
import NoThunks.Class
    ( NoThunks )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

import qualified Data.Monoid.Null as C

--------------------------------------------------------------------------------
-- Non-null values
--------------------------------------------------------------------------------

newtype NonNull v = UnsafeNonNull v
    deriving newtype NFData

-- Without an explicit type role annotation, the following role would be
-- inferred:
--
--     type role NonNull representational
--
-- However, the validity of a 'NonNull' value depends on a property of its
-- type: its 'mempty' value. A representational role would allow users to
-- coerce between types with the same representation but different 'mempty'
-- values, even without access to the 'NonNull' constructor.
--
-- For example, users could coerce a 'NonNull' ('Sum' 'Int') to a 'NonNull'
-- ('Product' 'Int'), and thereby construct a 'NonNull' value that is 'mempty'.
--
-- So we must instead choose a nominal role.
--
type role NonNull nominal

maybeNonNull :: MonoidNull v => v -> Maybe (NonNull v)
maybeNonNull !v
    | C.null  v = Nothing
    | otherwise = Just (UnsafeNonNull v)
{-# INLINE maybeNonNull #-}

applyNonNull :: (v -> a) -> (NonNull v -> a)
applyNonNull = coerce
{-# INLINE applyNonNull #-}

applyNonNull2 :: (v1 -> v2 -> a) -> (NonNull v1 -> NonNull v2 -> a)
applyNonNull2 = coerce
{-# INLINE applyNonNull2 #-}

fromNonNullMap :: Map k (NonNull v) -> Map k v
fromNonNullMap = coerce
{-# INLINE fromNonNullMap #-}

-- | /Unsafely/ converts an ordinary map into a map of non-null values.
--
-- This function /expects/ but does /not/ check that no value in the given map
-- is 'C.null'. It must only be applied to maps that satisfy this condition.
--
unsafeToNonNullMap :: Map k v -> Map k (NonNull v)
unsafeToNonNullMap = coerce
{-# INLINE unsafeToNonNullMap #-}

--------------------------------------------------------------------------------
-- Non-null maps
--------------------------------------------------------------------------------

-- A map of non-null values.
--
-- Types with the same representation as 'NonNullMap' (such as 'MonoidMap')
-- can derive their instances via this type, without needing access to the
-- constructor of 'NonNull'.
--
newtype NonNullMap k v = NonNullMap (Map k (NonNull v))
    deriving (Eq, Show, NFData, NoThunks) via Map k v
    deriving (Eq1, Show1, Foldable) via Map k
    deriving (Eq2, Show2, Bifoldable) via Map

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Constructs a map of non-null values from an ordinary 'Map', removing all
--   'C.null' values.
--
-- This function selects which of the @fromMap_*@ implementations below is used
-- by the public @fromMap@ function of the @monoidmap@ package.
--
fromMap :: MonoidNull v => Map k v -> Map k (NonNull v)
fromMap = fromMap_filter_if_any_null
{-# INLINE fromMap #-}

fromMap_filter :: MonoidNull v => Map k v -> Map k (NonNull v)
fromMap_filter = unsafeToNonNullMap . Map.filter (\(!v) -> not (C.null v))
{-# INLINABLE fromMap_filter #-}

fromMap_filter_if_any_null :: MonoidNull v => Map k v -> Map k (NonNull v)
fromMap_filter_if_any_null m
    | F.any (\(!v) -> C.null v) m =
        unsafeToNonNullMap $ Map.filter (\(!v) -> not (C.null v)) m
    | otherwise =
        unsafeToNonNullMap m
{-# INLINABLE fromMap_filter_if_any_null #-}

fromMap_mapMaybe :: MonoidNull v => Map k v -> Map k (NonNull v)
fromMap_mapMaybe = Map.mapMaybe maybeNonNull
{-# INLINABLE fromMap_mapMaybe #-}

-- | Constructs a map of non-null values from an ordinary 'Map', applying the
--   given function to all values, and removing all 'C.null' values.
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
