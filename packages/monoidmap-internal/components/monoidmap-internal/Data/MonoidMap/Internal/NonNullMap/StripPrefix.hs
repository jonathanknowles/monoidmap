-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'stripPrefix' function for 'NonNullMap'.
--
module Data.MonoidMap.Internal.NonNullMap.StripPrefix
    ( stripPrefix
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
    ( LeftReductive )

import qualified Data.MonoidMap.Internal.NonNullMap.Merge as Merge
import qualified Data.Semigroup.Cancellative as C

-- | Strips a prefix from a map.
--
-- This function is used by the public @stripPrefix@ function of the
-- @monoidmap@ package.
--
stripPrefix
    :: (Ord k, MonoidNull v, LeftReductive v)
    => NonNullMap k v
    -> NonNullMap k v
    -> Maybe (NonNullMap k v)
stripPrefix = Merge.mergeA Merge.Strategy
    { withNonNullL =
        Merge.withNonNullA (\v -> C.stripPrefix v mempty)

    , withNonNullR =
        Merge.keepNonNull
        -- Justification:
        --
        -- stripPrefix mempty a ≡ a

    , withNonNullP =
        Merge.withBothA C.stripPrefix
    }
{-# INLINE stripPrefix #-}
