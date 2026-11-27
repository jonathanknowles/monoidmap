{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @fromList@.
--
module Data.MonoidMap.Internal.NonNullMap.FromList
    ( fromList_cons_foldl1_flip_filter_if_any_null
    , fromList_cons_mconcat_reverse_filter_if_any_null
    , fromList_endo_filter_if_any_null
    , fromList_flip_filter_if_any_null
    , fromList_foldr_filter_if_any_null
    , fromList_reverse_filter_if_any_null
    , fromList_sort_group_sconcat_filter_if_any_null
    , fromList_foldl_cons_foldl1_flip_filter_if_any_null
    , fromList_foldl_flip_filter_if_any_null
    )
    where

import Prelude

import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.FromMap
    ( fromMap_filter_if_any_null, fromMap_mapMaybe )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Endo (..) )
import Data.Ord
    ( comparing )
import Data.Semigroup
    ( sconcat )

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map

fromList_flip_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_flip_filter_if_any_null =
    fromMap_filter_if_any_null . Map.fromListWith (flip (<>))
{-# INLINE fromList_flip_filter_if_any_null #-}

fromList_reverse_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_reverse_filter_if_any_null =
      fromMap_filter_if_any_null
    . Map.fromListWith (<>)
    . reverse
{-# INLINE fromList_reverse_filter_if_any_null #-}

fromList_foldr_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_foldr_filter_if_any_null =
      fromMap_filter_if_any_null
    . foldr (\(k, v) -> Map.insertWith (<>) k v) Map.empty
{-# INLINE fromList_foldr_filter_if_any_null #-}

-- Relies on 'List.sortBy' being stable, so that the values for each key stay
-- in order of appearance.
--
fromList_sort_group_sconcat_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_sort_group_sconcat_filter_if_any_null =
      fromMap_filter_if_any_null
    . Map.fromDistinctAscList
    . fmap (\kvs -> (fst (NonEmpty.head kvs), sconcat (snd <$> kvs)))
    . NonEmpty.groupWith fst
    . List.sortBy (comparing fst)
{-# INLINE fromList_sort_group_sconcat_filter_if_any_null #-}

fromList_cons_foldl1_flip_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_cons_foldl1_flip_filter_if_any_null =
      fromMap_filter_if_any_null
    . Map.map (List.foldl1' (flip (<>)))
    . collectNewestFirst
{-# INLINE fromList_cons_foldl1_flip_filter_if_any_null #-}

fromList_cons_mconcat_reverse_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_cons_mconcat_reverse_filter_if_any_null =
      fromMap_filter_if_any_null
    . Map.map (mconcat . reverse)
    . collectNewestFirst
{-# INLINE fromList_cons_mconcat_reverse_filter_if_any_null #-}

fromList_endo_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_endo_filter_if_any_null =
      fromMap_filter_if_any_null
    . Map.map (`appEndo` mempty)
    . Map.fromListWith (flip (<>))
    . fmap (fmap (Endo . (<>)))
{-# INLINE fromList_endo_filter_if_any_null #-}

collectNewestFirst :: Ord k => [(k, v)] -> Map k [v]
collectNewestFirst = Map.fromListWith (++) . fmap (fmap pure)
{-# INLINE collectNewestFirst #-}

fromList_foldl_cons_foldl1_flip_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_foldl_cons_foldl1_flip_filter_if_any_null =
      fromMap_filter_if_any_null
    . Map.map (List.foldl1' (flip (<>)))
    . List.foldl' (\m (k, v) -> Map.insertWith (++) k [v] m) Map.empty
{-# INLINE fromList_foldl_cons_foldl1_flip_filter_if_any_null #-}

fromList_foldl_flip_filter_if_any_null
    :: (Ord k, MonoidNull v) => [(k, v)] -> NonNullMap k v
fromList_foldl_flip_filter_if_any_null =
      fromMap_filter_if_any_null
    . List.foldl' (\m (k, v) -> Map.insertWith (flip (<>)) k v m) Map.empty
{-# INLINE fromList_foldl_flip_filter_if_any_null #-}
