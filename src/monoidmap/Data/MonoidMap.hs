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

    -- * Queries
    , keysSet
    , lookup
    , member
    , null
    , size

    -- * Modification
    , adjust
    , delete
    , insert
    , insertWith

    -- * Traversal
    , map
    )
    where

import Data.MonoidMap.Internal
import Prelude hiding
    ( lookup, map, null )
