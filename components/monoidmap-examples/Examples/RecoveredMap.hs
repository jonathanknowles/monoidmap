{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
-- An ordinary left-biased map similar to 'Map', implemented in terms of
-- 'MonoidMap'.
--
module Examples.RecoveredMap
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
    , traverse
    )
    where

import Prelude hiding
    ( lookup, map, traverse )

import Control.DeepSeq
    ( NFData )
import Data.Coerce
    ( coerce )
import Data.Maybe
    ( mapMaybe )
import Data.Monoid
    ( First (..) )
import Data.MonoidMap
    ( MonoidMap )
import Data.MonoidMapF
    ( MonoidMapF (MonoidMapF) )
import Data.Semigroup
    ( Semigroup (stimes), stimesIdempotentMonoid )
import Data.Set
    ( Set )

import qualified Data.MonoidMap as MonoidMap
import qualified Data.Traversable as Traversable

newtype Map k v = Map
    --  'First' is used to mimic the left-biased nature of 'Data.Map':
    (MonoidMapF k First v)
    deriving newtype (Eq, NFData, Monoid)
    deriving stock (Foldable, Functor, Traversable)

instance Ord k => Semigroup (Map k v) where
    (<>) = coerce @(MonoidMap k (First v) -> _ -> _) (<>)
    stimes = stimesIdempotentMonoid

instance (Show k, Show v) => Show (Map k v) where
    show = ("fromList " <>) . show . toList

toMap :: MonoidMap k (First v) -> Map k v
toMap = coerce

unMap :: Map k v -> MonoidMap k (First v)
unMap = coerce

empty :: Map k v
empty = toMap MonoidMap.empty

singleton :: Ord k => k -> v -> Map k v
singleton k = toMap . MonoidMap.singleton k . pure

fromList :: Ord k => [(k, v)] -> Map k v
fromList = toMap . MonoidMap.fromListWith (const id) . fmap (fmap pure)

toList :: Map k v -> [(k, v)]
toList = mapMaybe (getFirst . sequenceA) . MonoidMap.toList . unMap

delete :: Ord k => k -> Map k v -> Map k v
delete k = toMap . MonoidMap.nullify k . unMap

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v = toMap . MonoidMap.set k (pure v) . unMap

keysSet :: Map k v -> Set k
keysSet = MonoidMap.nonNullKeys . unMap

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k = getFirst . MonoidMap.get k . unMap

member :: Ord k => k -> Map k v -> Bool
member k = MonoidMap.nonNullKey k . unMap

map :: (v1 -> v2) -> Map k v1 -> Map k v2
map = fmap

mapWithKey :: (k -> v1 -> v2) -> Map k v1 -> Map k v2
mapWithKey f = toMap . MonoidMap.mapWithKey (fmap . f) . unMap

mapAccumL :: (s -> v1 -> (s, v2)) -> s -> Map k v1 -> (s, Map k v2)
mapAccumL = Traversable.mapAccumL

mapAccumR :: (s -> v1 -> (s, v2)) -> s -> Map k v1 -> (s, Map k v2)
mapAccumR = Traversable.mapAccumR

mapAccumLWithKey :: (s -> k -> v1 -> (s, v2)) -> s -> Map k v1 -> (s, Map k v2)
mapAccumLWithKey f s m =
    toMap <$> MonoidMap.mapAccumLWithKey (accumWithKey f) s (unMap m)

mapAccumRWithKey :: (s -> k -> v1 -> (s, v2)) -> s -> Map k v1 -> (s, Map k v2)
mapAccumRWithKey f s m =
    toMap <$> MonoidMap.mapAccumRWithKey (accumWithKey f) s (unMap m)

traverse :: Applicative f => (v1 -> f v2) -> Map k v1 -> f (Map k v2)
traverse = Traversable.traverse

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

accumWithKey :: (s -> k -> v1 -> (s, v2)) -> s -> k -> First v1 -> (s, First v2)
accumWithKey f s1 k (First mv1) = case mv1 of
    Just v1 -> let (s2, v2) = f s1 k v1 in (s2, First (Just v2))
    Nothing -> (s1, First Nothing)
