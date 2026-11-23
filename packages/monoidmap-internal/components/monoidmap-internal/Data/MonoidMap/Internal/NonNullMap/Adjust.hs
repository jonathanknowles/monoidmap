{-# LANGUAGE MagicHash #-}
{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides alternative implementations of @adjust@.
--
module Data.MonoidMap.Internal.NonNullMap.Adjust
    ( adjust_alter
    , adjust_alterF
    , adjust_lookup
    )
    where

import Prelude

import Data.Maybe
    ( fromMaybe )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal.NonNullMap.Unsafe
    ( NonNullMap (..) )
import GHC.Exts
    ( isTrue#, reallyUnsafePtrEquality# )

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

data Change v = Unchanged | Changed !v

instance Functor Change where
    fmap _ Unchanged   = Unchanged
    fmap f (Changed v) = Changed (f v)

adjust_alterF
    :: (Ord k, MonoidNull v)
    => (v -> v)
    -> k
    -> NonNullMap k v
    -> NonNullMap k v
adjust_alterF f k (NonNullMap m) = NonNullMap $
    case Map.alterF g k m of
        Unchanged  -> m
        Changed m1 -> m1
  where
    g Nothing =
        case NonNull.nonNull (f mempty) of
            Nothing -> Unchanged
            Just v1 -> Changed (Just v1)
    g (Just v0) =
        case NonNull.nonNull (NonNull.apply f v0) of
            Nothing -> Changed Nothing
            Just v1
                | v1 `ptrEq` v0 -> Unchanged
                | otherwise     -> Changed (Just v1)
{-# INLINE adjust_alterF #-}

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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

ptrEq :: v -> v -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# INLINE ptrEq #-}
