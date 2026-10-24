{-# LANGUAGE RoleAnnotations #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Defines the 'NonNullMap' type, together with the primitive operations on
-- 'NonNullMap' that need access to the data constructor of 'NonNull'.
--
module Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap (..)
    , toMap
    , unsafeFromMap
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
import Data.MonoidMap.Internal.NonNull.Unsafe
    ( NonNull (UnsafeNonNull) )
import NoThunks.Class
    ( NoThunks )

-- | A map of 'NonNull' values.
--
-- Other types with the same representation (such as @MonoidMap@ from the
-- @monoidmap@ package) can derive instances via this type, without access
-- to the data constructor of 'NonNull'.
--
newtype NonNullMap k v = NonNullMap (Map k (NonNull v))
    --
    -- Here we take great care to only derive instances that consume values.
    --
    -- It's risky to derive an instance that produces values, as the derived
    -- instance may not necessarily respect the 'NonNull' invariant.
    --
    -- For example, the 'Read' instance for 'Map', given appropriate input,
    -- will quite happily produce maps with 'mempty' values.
    --
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

-- | Converts a 'NonNullMap' into an ordinary map.
--
toMap :: NonNullMap k v -> Map k v
toMap = coerce
{-# INLINE toMap #-}

-- | /Unsafely/ converts an ordinary map into a 'NonNullMap'.
--
-- This function /expects/ but does /not/ check that no value in the given map
-- is null. It must only be applied to maps that satisfy this condition.
--
unsafeFromMap :: Map k v -> NonNullMap k v
unsafeFromMap = coerce
{-# INLINE unsafeFromMap #-}
