-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
-- Total monoidal map type with support for semigroup and monoid subclasses.
--
module Data.MonoidMap
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

    -- * Basic operations
    , get
    , set
    , adjust
    , delete

    -- * Queries
    , keys
    , member
    , notMember
    , null
    , notNull
    , size

    -- * Slicing
    , take
    , drop
    , splitAt

    -- * Filtering
    , filter
    , filterKeys
    , filterValues

    -- * Partitioning
    , partition
    , partitionKeys
    , partitionValues

    -- * Traversal
    , map
    , mapWith
    , mapKeys
    , mapKeysWith
    , mapValues
    )
    where

import Data.MonoidMap.Internal
import Prelude hiding
    ( drop, filter, map, null, splitAt, take )
