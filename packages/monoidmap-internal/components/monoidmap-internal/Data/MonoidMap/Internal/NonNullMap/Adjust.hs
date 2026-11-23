{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @adjust@.
--
module Data.MonoidMap.Internal.NonNullMap.Adjust
    ( adjust_alter
    , adjust_lookup
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

adjust_lookup
    :: (Ord k, MonoidNull v)
    => (v -> v)
    -> k
    -> NonNullMap k v
    -> NonNullMap k v
adjust_lookup f k (NonNullMap m) = NonNullMap $
    case Map.lookup k m of
        Nothing ->
            case NonNull.nonNull (f mempty) of
                Just v1 -> Map.insert k v1 m
                Nothing -> m
        Just v0 ->
            case NonNull.nonNull (NonNull.apply f v0) of
                Just v1 -> Map.insert k v1 m
                Nothing -> Map.delete k    m
{-# INLINE adjust_lookup #-}
