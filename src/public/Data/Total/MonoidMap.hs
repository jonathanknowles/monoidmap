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
    , filterWithKey
    , filterKeys
    , filter

    -- * Partitioning
    , partitionWithKey
    , partitionKeys
    , partitionValues

    -- * Mapping
    , map
    , mapKeys
    , mapKeysWith

    -- * Association
    , append

    -- * Subtraction
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

    -- * GCD
    , gcd
    )
    where

import Data.Total.MonoidMap.Internal
