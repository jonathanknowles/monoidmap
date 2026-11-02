-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'commonPrefix' function for 'NonNullMap'.
--
module Data.MonoidMap.Internal.NonNullMap.CommonPrefix
    ( commonPrefix
    )
    where

import Prelude

import Data.Monoid.GCD
    ( LeftGCDMonoid )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.Merge
    ( Strategy (withNonNullL, withNonNullR, withNonNullP) )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap )

import qualified Data.Monoid.GCD as C
import qualified Data.MonoidMap.Internal.NonNullMap.Merge as Merge

-- | Finds the greatest common prefix of two maps.
--
-- This function is used by the public @commonPrefix@ function of the
-- @monoidmap@ package.
--
commonPrefix
    :: (Ord k, MonoidNull v, LeftGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> NonNullMap k v
commonPrefix = Merge.merge Merge.Strategy
    { withNonNullL =
        Merge.keepNull
        -- Justification:
        --
        -- commonPrefix a mempty ≡ mempty

    , withNonNullR =
        Merge.keepNull
        -- Justification:
        --
        -- commonPrefix mempty a ≡ mempty

    , withNonNullP =
        Merge.withBoth C.commonPrefix
    }
{-# INLINE commonPrefix #-}
