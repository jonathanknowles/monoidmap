-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'NonNull' type of values that are not 'null', and functions for
-- working with 'NonNull' values.
--
module Data.MonoidMap.Internal.NonNull
    (
    -- * Type
      NonNull

    -- * Construction
    , maybeNonNull

    -- * Deconstruction
    , unwrap

    -- * Function application
    , applyNonNull
    , applyNonNull2
    )
    where

import Prelude hiding
    ( null )

import Data.Monoid.Null
    ( MonoidNull (null) )
import Data.MonoidMap.Internal.NonNull.Unsafe
    ( NonNull, maybeNonNull, unwrap )

applyNonNull :: (v -> a) -> (NonNull v -> a)
applyNonNull f = f . unwrap
{-# INLINE applyNonNull #-}

applyNonNull2 :: (v1 -> v2 -> a) -> (NonNull v1 -> NonNull v2 -> a)
applyNonNull2 f v1 v2 = f (unwrap v1) (unwrap v2)
{-# INLINE applyNonNull2 #-}
