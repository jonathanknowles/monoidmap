-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'map' function for 'NonNullMap'.
--
module Data.MonoidMap.Internal.NonNullMap.Map
    ( map
    )
    where

import Prelude hiding
    ( map )

import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap (..) )

import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap.Internal.NonNull as NonNull

-- | Applies the given function to all values, removing all values that are
--   @null@.
--
-- This function is used by the public @map@ function of the @monoidmap@
-- package.
--
map
    :: MonoidNull v2
    => (v1 -> v2)
    -> NonNullMap k v1
    -> NonNullMap k v2
map f (NonNullMap m) =
    NonNullMap $ Map.mapMaybe (NonNull.nonNull . NonNull.apply f) m
