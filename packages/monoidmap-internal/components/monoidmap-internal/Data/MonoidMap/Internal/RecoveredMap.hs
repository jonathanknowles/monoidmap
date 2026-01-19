{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- An ordinary left-biased map similar to 'Map', implemented in terms of
-- 'MonoidMap'.
--
module Data.MonoidMap.Internal.RecoveredMap
    ( Map
    , empty
    , singleton
    , fromList
    , toList
    , delete
    , insert
    , keysSet
    , lookup
    , member
    , map
    , mapWithKey
    , mapAccumL
    , mapAccumLWithKey
    , mapAccumR
    , mapAccumRWithKey
    )
    where

import Prelude hiding
    ( lookup, map )

import Control.DeepSeq
    ( NFData )
import Data.Coerce
    ( coerce )
import Data.Maybe
    ( mapMaybe )
import Data.Monoid
    ( First (..) )
import Data.MonoidMap.Internal
    ( MonoidMap )
import Data.Semigroup
    ( Semigroup (stimes), stimesIdempotentMonoid )
import Data.Set
    ( Set )

import qualified Data.MonoidMap.Internal as MonoidMap

newtype Map k v = Map
    --  'First' is used to mimic the left-biased nature of 'Data.Map':
    {unMap :: MonoidMap k (First v)}
    deriving newtype (Eq, NFData, Monoid)

instance Ord k => Semigroup (Map k v) where
    (<>) = coerce @(MonoidMap k (First v) -> _ -> _) (<>)
    stimes = stimesIdempotentMonoid

instance (Show k, Show v) => Show (Map k v) where
    show = ("fromList " <>) . show . toList

instance Functor (Map k) where
    fmap = map

empty :: Map k v
empty = Map MonoidMap.empty

singleton :: Ord k => k -> v -> Map k v
singleton k = Map . MonoidMap.singleton k . pure

fromList :: Ord k => [(k, v)] -> Map k v
fromList = Map . MonoidMap.fromListWith (const id) . fmap (fmap pure)

toList :: Map k v -> [(k, v)]
toList = mapMaybe (getFirst . sequenceA) . MonoidMap.toList . unMap

delete :: Ord k => k -> Map k v -> Map k v
delete k = Map . MonoidMap.nullify k . unMap

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v = Map . MonoidMap.set k (pure v) . unMap

keysSet :: Map k v -> Set k
keysSet = MonoidMap.nonNullKeys . unMap

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k = getFirst . MonoidMap.get k . unMap

member :: Ord k => k -> Map k v -> Bool
member k = MonoidMap.nonNullKey k . unMap

map :: (v1 -> v2) -> Map k v1 -> Map k v2
map f = Map . MonoidMap.map (fmap f) . unMap

mapWithKey :: (k -> v1 -> v2) -> Map k v1 -> Map k v2
mapWithKey f = Map . MonoidMap.mapWithKey (fmap . f) . unMap

mapAccumL :: (s -> v1 -> (s, v2)) -> s -> Map k v1 -> (s, Map k v2)
mapAccumL f s m = Map <$> MonoidMap.mapAccumL (accum f) s (unMap m)

mapAccumR :: (s -> v1 -> (s, v2)) -> s -> Map k v1 -> (s, Map k v2)
mapAccumR f s m = Map <$> MonoidMap.mapAccumR (accum f) s (unMap m)

mapAccumLWithKey :: (s -> k -> v1 -> (s, v2)) -> s -> Map k v1 -> (s, Map k v2)
mapAccumLWithKey f s m =
    Map <$> MonoidMap.mapAccumLWithKey (accumWithKey f) s (unMap m)

mapAccumRWithKey :: (s -> k -> v1 -> (s, v2)) -> s -> Map k v1 -> (s, Map k v2)
mapAccumRWithKey f s m =
    Map <$> MonoidMap.mapAccumRWithKey (accumWithKey f) s (unMap m)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

accum :: (s -> v1 -> (s, v2)) -> s -> First v1 -> (s, First v2)
accum f s1 (First mv1) = case mv1 of
    Just v1 -> let (s2, v2) = f s1 v1 in (s2, First (Just v2))
    Nothing -> (s1, First Nothing)

accumWithKey :: (s -> k -> v1 -> (s, v2)) -> s -> k -> First v1 -> (s, First v2)
accumWithKey f s1 k (First mv1) = case mv1 of
    Just v1 -> let (s2, v2) = f s1 k v1 in (s2, First (Just v2))
    Nothing -> (s1, First Nothing)
