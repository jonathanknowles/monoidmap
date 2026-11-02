-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'commonSuffix' function for 'NonNullMap'.
--
module Data.MonoidMap.Internal.NonNullMap.CommonSuffix
    ( commonSuffix
    )
    where

import Prelude

import Data.Monoid.GCD
    ( RightGCDMonoid )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.Merge
    ( Strategy (withNonNullL, withNonNullR, withNonNullP) )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap )

import qualified Data.Monoid.GCD as C
import qualified Data.MonoidMap.Internal.NonNullMap.Merge as Merge

-- | Finds the greatest common suffix of two maps.
--
-- This function is used by the public @commonSuffix@ function of the
-- @monoidmap@ package.
--
commonSuffix
    :: (Ord k, MonoidNull v, RightGCDMonoid v)
    => NonNullMap k v
    -> NonNullMap k v
    -> NonNullMap k v
commonSuffix = Merge.merge Merge.Strategy
    { withNonNullL =
        Merge.keepNull
        -- Justification:
        --
        -- commonSuffix a mempty ≡ mempty

    , withNonNullR =
        Merge.keepNull
        -- Justification:
        --
        -- commonSuffix mempty a ≡ mempty

    , withNonNullP =
        Merge.withBoth C.commonSuffix
    }
{-# INLINE commonSuffix #-}
