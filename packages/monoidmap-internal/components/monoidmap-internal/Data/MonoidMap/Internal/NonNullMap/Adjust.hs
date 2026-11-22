{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @adjust@.
--
module Data.MonoidMap.Internal.NonNullMap.Adjust
    ( adjust_alter
    )
    where

import Prelude

import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap (..) )

import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap.Internal.NonNull as NonNull

adjust_alter
    :: (Ord k, MonoidNull v)
    => (v -> v)
    -> k
    -> NonNullMap k v
    -> NonNullMap k v
adjust_alter f k (NonNullMap m) = NonNullMap $
    Map.alter (NonNull.nonNull . maybe (f mempty) (NonNull.apply f)) k m
{-# INLINE adjust_alter #-}
