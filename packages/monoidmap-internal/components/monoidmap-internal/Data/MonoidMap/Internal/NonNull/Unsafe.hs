{-# LANGUAGE RoleAnnotations #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Defines the 'NonNull' type and the primitive operations that require
-- access to its data constructor.
--
-- This module is the only module with access to the data constructor of
-- 'NonNull', and is therefore the only module that can construct a 'NonNull'
-- value that is 'C.null'. It is not exposed by this package.
--
-- All other modules must construct 'NonNull' values with the functions
-- defined here.
--
module Data.MonoidMap.Internal.NonNull.Unsafe
    ( NonNull
    , NonNullMap (..)
    , maybeNonNull
    , applyNonNull
    , applyNonNull2
    , fromNonNullMap
    , unsafeToNonNullMap
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

-- Strictly speaking, this type role annotation is redundant, as:
--
-- - The type parameter 'k' already has a nominal role inherited from 'Map'.
-- - The type parameter 'v' already has a nominal role inherited from 'NonNull'.
--
-- But we reassert these roles here, so that they cannot be silently weakened
-- by a future change to the representation of 'NonNullMap'.
--
type role NonNullMap nominal nominal
