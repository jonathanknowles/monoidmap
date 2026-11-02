-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'stripSuffix' function for 'NonNullMap'.
--
module Data.MonoidMap.Internal.NonNullMap.StripSuffix
    ( stripSuffix
    )
    where

import Prelude

import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.Merge
    ( Strategy (withNonNullL, withNonNullR, withNonNullP) )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap )
import Data.Semigroup.Cancellative
    ( RightReductive )

import qualified Data.MonoidMap.Internal.NonNullMap.Merge as Merge
import qualified Data.Semigroup.Cancellative as C

-- | Strips a suffix from a map.
--
-- This function is used by the public @stripSuffix@ function of the
-- @monoidmap@ package.
--
stripSuffix
    :: (Ord k, MonoidNull v, RightReductive v)
    => NonNullMap k v
    -> NonNullMap k v
    -> Maybe (NonNullMap k v)
stripSuffix = Merge.mergeA Merge.Strategy
    { withNonNullL =
        Merge.withNonNullA (\v -> C.stripSuffix v mempty)

    , withNonNullR =
        Merge.keepNonNull
        -- Justification:
        --
        -- stripSuffix mempty a ≡ a

    , withNonNullP =
        Merge.withBothA C.stripSuffix
    }
{-# INLINE stripSuffix #-}
