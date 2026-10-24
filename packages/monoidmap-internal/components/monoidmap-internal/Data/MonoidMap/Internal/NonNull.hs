-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'NonNull' type of values that are not 'null', and functions for
-- working with 'NonNull' values.
--
-- This module is intended to be imported qualified:
--
-- @
-- import Data.MonoidMap.Internal.NonNull (NonNull)
-- import Data.MonoidMap.Internal.NonNull qualified as NonNull
-- @
--
module Data.MonoidMap.Internal.NonNull
    (
    -- * Type
      NonNull

    -- * Construction
    , nonNull

    -- * Deconstruction
    , unwrap

    -- * Function application
    , apply
    , apply2
    )
    where

import Prelude hiding
    ( null )

import Data.Monoid.Null
    ( MonoidNull (null) )
import Data.MonoidMap.Internal.NonNull.Unsafe
    ( NonNull, nonNull, unwrap )

-- | Applies a function on ordinary values to a 'NonNull' value.
--
apply :: (v -> a) -> (NonNull v -> a)
apply f = f . unwrap
{-# INLINE apply #-}

-- | Applies a binary function on ordinary values to two 'NonNull' values.
--
apply2 :: (v1 -> v2 -> a) -> (NonNull v1 -> NonNull v2 -> a)
apply2 f v1 v2 = f (unwrap v1) (unwrap v2)
{-# INLINE apply2 #-}
