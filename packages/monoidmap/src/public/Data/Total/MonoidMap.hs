-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'MonoidMap' type, which models a __total function__ from
-- keys to __monoidal__ values.
--
module Data.Total.MonoidMap
    (
    -- * Type
      MonoidMap

    -- * Construction
    , empty
    , fromList
    , fromListWith
    , fromMap
    , singleton

    -- * Deconstruction
    , toList
    , toMap

    -- * Lookup
    , get

    -- * Modification
    , set
    , adjust
    , nullify

    -- * Membership
    , null
    , nullKey
    , nonNull
    , nonNullCount
    , nonNullKey
    , nonNullKeys

    -- * Slicing
    , take
    , drop
    , splitAt

    -- * Filtering
    , filter
    , filterKeys
    , filterWithKey

    -- * Partitioning
    , partition
    , partitionKeys
    , partitionWithKey

    -- * Mapping
    , map
    , mapKeys
    , mapKeysWith

    -- * Comparison
    , isSubmapOf
    , isSubmapOfBy
    , disjoint
    , disjointBy

    -- * Intersection
    , intersection
    , intersectionWith
    , intersectionWithA

    -- * Union
    , union
    , unionWith
    , unionWithA

    -- * Association
    , append

    -- * Reduction
    , minus
    , minusMaybe
    , monus

    -- * Inversion
    , invert

    -- * Exponentiation
    , power

    -- * Prefixes
    , isPrefixOf
    , stripPrefix
    , commonPrefix
    , stripCommonPrefix

    -- * Suffixes
    , isSuffixOf
    , stripSuffix
    , commonSuffix
    , stripCommonSuffix

    -- * Overlap
    , overlap
    , stripPrefixOverlap
    , stripSuffixOverlap
    , stripOverlap
    )
    where

import Data.Total.MonoidMap.Internal
