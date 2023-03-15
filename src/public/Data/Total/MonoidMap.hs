-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- Total monoidal map type with support for semigroup and monoid subclasses.
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

    -- * Merging
    , intersectionWith
    , intersectionWithA
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

    -- * Bounds
    , gcd
    , lcm

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
