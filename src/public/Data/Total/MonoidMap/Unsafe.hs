{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides /unsafe/ operations for the 'MonoidMap' type.
--
module Data.Total.MonoidMap.Unsafe
    (
    -- * Construction
      unsafeFromMap
    )
    where

import Prelude

import Data.Coerce
    ( coerce )
import Data.Map.Strict
    ( Map )
import Data.Total.MonoidMap.Internal
    ( MonoidMap (..), NonNull (..), fromMap )

import qualified Data.Foldable as F
import qualified Data.Monoid.Null as Null
import qualified Data.Total.MonoidMap.Internal as Internal

--------------------------------------------------------------------------------
-- Unsafe construction
--------------------------------------------------------------------------------

-- | \(O(1)\). /Unsafely/ constructs a 'MonoidMap' from an ordinary 'Map'.
--
-- Constructs a 'MonoidMap' in /constant time/, without imposing the burden
-- of a canonicalisation step to remove 'null' values.
--
-- When applied to a given 'Map' @m@, this function /expects/ but does /not/
-- check the following pre-condition:
--
-- @
-- 'F.all' ('not' . 'Null.null') m
-- @
--
-- Not satisfying this pre-condition will result in undefined behaviour.
--
-- See 'fromMap' for a safe version of this function.
--
unsafeFromMap :: Map k v -> MonoidMap k v
unsafeFromMap = coerce
