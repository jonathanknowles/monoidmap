-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @fromList@.
--
module Data.MonoidMap.Internal.NonNullMap.FromList
    ( fromList
    )
    where

import Prelude

import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.FromMap
    ( fromMap_filter_if_any_null )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap )

import qualified Data.Map.Strict as Map

fromList
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList = fromMap_filter_if_any_null . Map.fromListWith (flip (<>))
{-# INLINE fromList #-}
