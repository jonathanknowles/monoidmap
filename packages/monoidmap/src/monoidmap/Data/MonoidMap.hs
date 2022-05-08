-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap
    (
    -- * Type
      MonoidMap

    -- * Construction
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
    )
    where

import Data.MonoidMap.Internal
import Prelude hiding
    ( lookup, null )
