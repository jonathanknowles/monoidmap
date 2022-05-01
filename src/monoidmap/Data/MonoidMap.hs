-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap
    (
--  * Type
      MonoidMap

--  * Construction
    , fromMap
    , singleton

--  * Deconstruction
    , toMap

--  * Queries
    , keysSet
    , lookup
    , member
    , size

--  * Modification
    , adjust
    , delete
    , insert
    )
    where

import Data.MonoidMap.Internal
import Prelude hiding
    ( lookup )
