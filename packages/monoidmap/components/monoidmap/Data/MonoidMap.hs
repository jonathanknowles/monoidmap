{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap
    (
    -- * Introduction
    -- $_introduction
    -- $_totality
    -- $_encoding
    -- $_monoidal_operations

    -- * Type
      MonoidMap

    -- * General operations

    -- ** Construction
    , empty
    , fromList
    , fromListWith
    , fromMap
    , fromMapWith
    , fromSet
    , singleton

    -- ** Deconstruction
    , toList
    , toMap

    -- ** Lookup
    , get

    -- ** Modification
    , set
    , adjust
    , nullify

    -- ** Membership
    , null
    , nullKey
    , nonNull
    , nonNullCount
    , nonNullKey
    , nonNullKeys

    -- ** Slicing
    , take
    , drop
    , splitAt

    -- ** Filtering
    , filter
    , filterKeys
    , filterWithKey

    -- ** Partitioning
    , partition
    , partitionKeys
    , partitionWithKey

    -- ** Mapping
    , map
    , mapKeys
    , mapKeysWith
    , mapWithKey

    -- ** Folding
    , foldl
    , foldl'
    , foldr
    , foldr'
    , foldlWithKey
    , foldlWithKey'
    , foldrWithKey
    , foldrWithKey'
    , foldMapWithKey
    , foldMapWithKey'

    -- ** Traversal
    , traverse
    , traverseWithKey
    , mapAccumL
    , mapAccumLWithKey
    , mapAccumR
    , mapAccumRWithKey

    -- * Monoidal operations

    -- | See the section on [monoidal operations](#_monoidal_operations) within
    -- the [introduction](#_introduction).

    -- ** Association
    , append

    -- ** Subtraction
    , minus
    , minusMaybe
    , monus

    -- ** Inversion
    , invert

    -- ** Exponentiation
    , power

    -- ** Comparison
    , isSubmapOf
    , isSubmapOfBy
    , disjoint
    , disjointBy

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithA

    -- ** Union
    , union
    , unionWith
    , unionWithA

    -- ** Prefixes
    , isPrefixOf
    , stripPrefix
    , commonPrefix
    , stripCommonPrefix

    -- ** Suffixes
    , isSuffixOf
    , stripSuffix
    , commonSuffix
    , stripCommonSuffix

    -- ** Overlap
    , overlap
    , stripPrefixOverlap
    , stripSuffixOverlap
    , stripOverlap
    )
    where

import Data.MonoidMap.Internal

-- Imports for module documentation:

import Data.Eq
    ( Eq (..) )
import Data.Group
    ( Group ((~~)) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( Maybe (Just, Nothing) )
import Data.Monoid
    ( Monoid (mempty) )
import Data.Monoid.GCD
    ( GCDMonoid )
import Data.Monoid.LCM
    ( LCMMonoid )
import Data.Monoid.Monus
    ( Monus ((<\>)) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.Ord
    ( Ord )
import Data.Semigroup
    ( Semigroup ((<>)) )

import qualified Data.Map.Strict as SMap
import qualified Data.MonoidMap.Internal as MMap

import qualified Data.Group as C
import qualified Data.Monoid.GCD as C
import qualified Data.Monoid.LCM as C
import qualified Data.Monoid.Null as C

--------------------------------------------------------------------------------
-- Introduction
--------------------------------------------------------------------------------

-- $_introduction
-- #_introduction#
--
-- This module provides the 'MonoidMap' type, which:
--
--  - models a [__total function__](#_totality) with
--    [__finite support__](https://wikipedia.org/wiki/Support_\(mathematics\))
--    from keys to [__monoidal values__]("Data.Monoid"), with a default value
--    of 'mempty'.
--
--  - encodes key-value mappings with a [__minimal encoding__](#_encoding) that
--    only includes values /not/ equal to 'mempty'.
--
--  - provides a comprehensive set of
--    [__monoidal operations__](#_monoidal_operations) for transforming,
--    combining, and comparing maps.
--
-- The documentation in this module serves as __reference guide__ to the
-- 'MonoidMap' type and its operations.
--
-- See the
-- [@README@](https://github.com/jonathanknowles/monoidmap/blob/main/README.md)
-- file for:
--
--  - a __deeper introduction__ to the design of the 'MonoidMap' type.
--
--  - worked examples of using 'MonoidMap' to implement more specialised
--    monoidal data structures.
--
--  - detailed comparisons between 'MonoidMap' and other map types.

--------------------------------------------------------------------------------
-- Totality
--------------------------------------------------------------------------------

-- $_totality
-- #_totality#
--
-- = Relationship between keys and values
--
-- A 'MonoidMap' of key type __@k@__ and value type __@v@__ associates /every/
-- possible key of type __@k@__ with a value of type __@v@__:
--
-- @
-- 'get' :: ('Ord' k, 'Monoid' v) => k -> 'MonoidMap' k v -> v
-- @
--
-- The 'empty' map associates every key __@k@__ with a default value of
-- 'mempty':
--
-- @
-- ∀ k. 'get' k 'empty' '==' 'mempty'
-- @
--
-- == Comparison with standard 'Map' type
--
-- The 'MonoidMap' type differs from the standard 'Map' type in how it relates
-- keys to values:
--
-- +---------------------------+---------------------------------------------+
-- | Type                      | Models a total function with finite support |
-- +===========================+=============================================+
-- | @       'Map'@__@ k v @__ | from keys of type __@k@__                   |
-- |                           | to values of type __@'Maybe' v@__.          |
-- +---------------------------+---------------------------------------------+
-- | @ 'MonoidMap'@__@ k v @__ | from keys of type __@k@__                   |
-- |                           | to values of type __@v@__.                  |
-- +---------------------------+---------------------------------------------+
--
-- This difference can be illustrated by comparing the type signatures of
-- operations to query a key for its value, for both types:
--
-- @
--       'Map'.'SMap.lookup' :: \      \      k ->       'Map' k v -> 'Maybe' v
-- 'MonoidMap'.'MMap.get'    :: 'Monoid' v => k -> 'MonoidMap' k v -> \     \ v
-- @
--
-- Whereas a standard 'Map' has a default value of 'Nothing', a 'MonoidMap' has
-- a default value of 'mempty':
--
-- @
-- ∀ k.       'Map'.'SMap.lookup' k       'Map'.'SMap.empty' '==' 'Nothing'
-- ∀ k. 'MonoidMap'.'MMap.get'    k 'MonoidMap'.'MMap.empty' '==' 'mempty'
-- @
--
-- In practice, the standard 'Map' type uses 'Maybe' to indicate the /presence/
-- or /absence/ of a value for a particular key. This representation is
-- necessary because the 'Map' type imposes no constraints on value types.
--
-- However, /monoidal/ types already have a natural way to represent null or
-- empty values: the 'mempty' constant, which represents the /neutral/ or
-- /identity/ element of a 'Monoid'.
--
-- Consequently, using a standard 'Map' with a /monoidal/ value type gives rise
-- to two distinct representations for null or empty values:
--
-- +-------------------------+--------------------------------------------+
-- | @                       |                                            |
-- | 'Map'.'SMap.lookup' k m | Interpretation                             |
-- | @                       |                                            |
-- +=========================+============================================+
-- | @                       | 'Map' __@m@__ has /no/ entry               |
-- | 'Nothing'               | for key __@k@__.                           |
-- | @                       |                                            |
-- +-------------------------+--------------------------------------------+
-- | @                       | 'Map' __@m@__ has an entry                 |
-- | 'Just' 'mempty'         | for key __@k@__, but the value is /empty/. |
-- | @                       |                                            |
-- +-------------------------+--------------------------------------------+
--
-- In contrast, the 'MonoidMap' type provides a single, /canonical/
-- representation for null or empty values, according to the following
-- conceptual mapping:
--
-- +------------------------------------+---+------------------------------+
-- | @                                  |   | @                            |
-- | 'Map'.'SMap.lookup' k m            |   | 'MonoidMap'.'MMap.get' k m   |
-- | @                                  |   | @                            |
-- +====================================+===+==============================+
-- | @                                  |   | @                            |
-- | 'Nothing'                          | ⟼ | 'mempty'                     |
-- | @                                  |   | @                            |
-- +--------------+---------------------+---+------------------------------+
-- | @            | @                   |   | @                            |
-- | 'Just' __v__ | __v__ '==' 'mempty' | ⟼ | 'mempty'                     |
-- | @            | @                   |   | @                            |
-- +--------------+---------------------+---+------------------------------+
-- | @            | @                   |   | @                            |
-- | 'Just' __v__ | __v__ '/=' 'mempty' | ⟼ | __v__                        |
-- | @            | @                   |   | @                            |
-- +--------------+---------------------+---+------------------------------+

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

-- $_encoding
-- #_encoding#
--
-- = Encoding
--
-- A 'MonoidMap' only encodes mappings from keys to values that are /not/ equal
-- to 'mempty'.
--
-- The total function \(T\) modelled by a 'MonoidMap' is encoded as a
-- __support__ __map__ \(S\), where \(S\) is the finite subset of key-value
-- mappings in \(T\) for which values are not equal to 'mempty' (denoted by
-- \(\varnothing\)):
--
-- \( \quad S = \{ (k, v) \in T \ | \ v \ne \varnothing \} \)
--
-- == Automatic minimisation
--
-- All 'MonoidMap' operations perform __automatic minimisation__ of the support
-- map, so that 'mempty' values do not appear in:
--
--   - any encoding of a 'MonoidMap'
--   - any traversal of a 'MonoidMap'
--
-- == Constraints on values
--
-- 'MonoidMap' operations require the monoidal value type to be an instance of
-- 'MonoidNull'.
--
-- Instances of 'MonoidNull' must provide a 'C.null' indicator function that
-- satisfies the following law:
--
-- @
-- ∀ v. 'MonoidNull'.'C.null' v '==' (v '==' 'mempty')
-- @
--
-- 'MonoidMap' operations use the 'C.null' indicator function to detect and
-- exclude 'mempty' values from the support map.
--
-- Note that it is /not/ generally necessary for the value type to be an
-- instance of 'Eq'.

--------------------------------------------------------------------------------
-- Monoidal operations
--------------------------------------------------------------------------------

-- $_monoidal_operations
-- #_monoidal_operations#
--
-- = Monoidal operations
--
-- The 'MonoidMap' type provides a comprehensive set of __monoidal operations__
-- for transforming, combining, and comparing maps.
--
-- Instances for several __subclasses__ of 'Semigroup' and 'Monoid' are
-- provided, including classes from the following libraries:
--
--  - [groups](https://hackage.haskell.org/package/groups)
--  - [monoid-subclasses](https://hackage.haskell.org/package/monoid-subclasses)
--
-- At the root of this hierarchy of subclasses is the 'Semigroup' class, whose
-- instance for 'MonoidMap' is defined in terms of the /underlying value type/,
-- so that applying `(<>)` to a /pair of maps/ is equivalent to applying `(<>)`
-- to all /pairs of values/ for matching keys:
--
-- @
-- ∀ k. 'get' k (m1 '<>' m2) '==' 'get' k m1 '<>' 'get' k m2
-- @
--
-- In general, operations for subclasses of 'Semigroup' and 'Monoid' are
-- defined /analogously/ to the 'Semigroup' instance, so that:
--
-- - /unary/ operations on /individual maps/ are defined in terms of their
--   distributive application to all values.
--
-- - /binary/ operations on /pairs of maps/ are defined in terms of their
--   distributive application to all /pairs of values/ for matching keys.
--
-- Unary monoidal operations typically satisfy a property similar to:
--
-- @
-- ∀ k. 'get' k (f m) '==' f ('get' k m)
-- @
--
-- Binary monoidal operations typically satisfy a property similar to:
--
-- @
-- ∀ k. 'get' k (f m1 m2) '==' f ('get' k m1) ('get' k m2)
-- @
--
-- Defining monoidal operations in this way makes it possible to transform,
-- combine, and compare maps in ways that are consistent with the algebraic
-- properties of the underlying monoidal value type.
