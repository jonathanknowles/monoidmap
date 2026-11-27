{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @fromList@.
--
-- Each implementation is named after the fold that its combining of the
-- values for each key is equivalent to (for example, @foldl1 (<>)@), and then
-- after the way that it removes null values.
--
module Data.MonoidMap.Internal.NonNullMap.FromList
    ( fromList_foldl1_filter_if_any_null
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

fromList_foldl1_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_foldl1_filter_if_any_null =
    fromMap_filter_if_any_null . Map.fromListWith (flip (<>))
{-# INLINE fromList_foldl1_filter_if_any_null #-}
