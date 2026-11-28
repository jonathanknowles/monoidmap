{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @fromList@.
--
-- Each implementation is named after:
--
-- - the fold that its combining of the values for each key is equivalent to
--   (for example, @foldl1 (<>)@);
-- - the structure in which it first collects the values for each key, if any
--   (for example, @list@); and
-- - the way that it removes null values.
--
module Data.MonoidMap.Internal.NonNullMap.FromList
    ( fromList_foldl1_filter_if_any_null
    , fromList_foldr1_list_filter_if_any_null
    , fromList_foldr1_seq_filter_if_any_null
    )
    where

import Prelude

import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.FromMap
    ( fromMap_filter_if_any_null )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap )

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

fromList_foldl1_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_foldl1_filter_if_any_null =
    fromMap_filter_if_any_null . Map.fromListWith (flip (<>))
{-# INLINE fromList_foldl1_filter_if_any_null #-}

fromList_foldr1_list_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_foldr1_list_filter_if_any_null =
      fromMap_filter_if_any_null
    . Map.map (List.foldl1' (flip (<>)))
    . List.foldl' (\m (k, v) -> Map.insertWith (++) k [v] m) Map.empty
{-# INLINE fromList_foldr1_list_filter_if_any_null #-}

fromList_foldr1_seq_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_foldr1_seq_filter_if_any_null =
      fromMap_filter_if_any_null
    . Map.map (foldr1 (<>))
    . List.foldl'
        (\m (k, v) -> Map.insertWith (flip (Seq.><)) k (Seq.singleton v) m)
        Map.empty
{-# INLINE fromList_foldr1_seq_filter_if_any_null #-}
