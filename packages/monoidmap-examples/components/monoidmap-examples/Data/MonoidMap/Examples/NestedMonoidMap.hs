{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- A nested map with compound keys, implemented in terms of 'MonoidMap'.
--
module Data.MonoidMap.Examples.NestedMonoidMap
    (
    -- * Type
      NestedMonoidMap

    -- * Construction
    , fromFlatList
    , fromFlatMap
    , fromNestedList
    , fromNestedMap

    -- * Deconstruction
    , toFlatList
    , toFlatMap
    , toNestedList
    , toNestedMap

    -- * Basic operations
    , get
    , set
    , adjust
    , nullify

    -- * Membership
    , nonNullCount
    , nonNullKey
    , nonNullKeys

    -- * Intersection
    , intersection
    , intersectionWith

    -- * Union
    , union
    , unionWith

    -- * Comparison
    , isSubmapOf
    , isSubmapOfBy
    , disjoint
    , disjointBy
    )
    where

import Prelude

import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, OverlappingGCDMonoid, RightGCDMonoid )
import Data.Monoid.LCM
    ( LCMMonoid )
import Data.Monoid.Monus
    ( Monus )
import Data.Monoid.Null
    ( MonoidNull, PositiveMonoid )
import Data.MonoidMap
    ( MonoidMap )
import Data.Semigroup.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive
    , Reductive
    , RightCancellative
    , RightReductive
    )
import Data.Set
    ( Set )
import GHC.Exts
    ( IsList (..) )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype NestedMonoidMap k1 k2 v =
    NestedMonoidMap (MonoidMap k1 (MonoidMap k2 v))
    deriving stock Eq
    deriving newtype
        ( Cancellative
        , Commutative
        , GCDMonoid
        , LCMMonoid
        , LeftCancellative
        , LeftGCDMonoid
        , LeftReductive
        , Monoid
        , MonoidNull
        , Monus
        , OverlappingGCDMonoid
        , PositiveMonoid
        , Reductive
        , RightCancellative
        , RightGCDMonoid
        , RightReductive
        , Semigroup
        , Show
        )

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

fromFlatList
    :: (Ord k1, Ord k2, MonoidNull v)
    => [((k1, k2), v)]
    -> NestedMonoidMap k1 k2 v
fromFlatList = F.foldl' acc mempty
  where
    acc m ((k1, k2), v) = adjust (<> v) k1 k2 m

fromFlatMap
    :: (Ord k1, Ord k2, MonoidNull v)
    => Map (k1, k2) v
    -> NestedMonoidMap k1 k2 v
fromFlatMap = fromFlatList . Map.toList

fromNestedList
    :: (Ord k1, Ord k2, MonoidNull v)
    => [(k1, [(k2, v)])]
    -> NestedMonoidMap k1 k2 v
fromNestedList entries =
    fromFlatList [((k1, k2), v) | (k1, n) <- entries, (k2, v) <- n]

fromNestedMap
    :: (Ord k2, MonoidNull v)
    => Map k1 (Map k2 v)
    -> NestedMonoidMap k1 k2 v
fromNestedMap = NestedMonoidMap . MonoidMap.fromMap . fmap MonoidMap.fromMap

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

toFlatList
    :: (Ord k1, Ord k2, MonoidNull v)
    => NestedMonoidMap k1 k2 v
    -> [((k1, k2), v)]
toFlatList m = [((k1, k2), v) | (k1, n) <- toNestedList m, (k2, v) <- toList n]

toFlatMap
    :: (Ord k1, Ord k2, MonoidNull v)
    => NestedMonoidMap k1 k2 v
    -> Map (k1, k2) v
toFlatMap = Map.fromList . toFlatList

toNestedList
    :: (Ord k1, Ord k2, MonoidNull v)
    => NestedMonoidMap k1 k2 v
    -> [(k1, [(k2, v)])]
toNestedList (NestedMonoidMap m) = fmap toList <$> toList m

toNestedMap
    :: NestedMonoidMap k1 k2 v
    -> Map k1 (Map k2 v)
toNestedMap (NestedMonoidMap m) = MonoidMap.toMap <$> MonoidMap.toMap m

--------------------------------------------------------------------------------
-- Basic operations
--------------------------------------------------------------------------------

get :: (Ord k1, Ord k2, MonoidNull v)
    => k1
    -> k2
    -> NestedMonoidMap k1 k2 v
    -> v
get k1 k2 (NestedMonoidMap m) = MonoidMap.get k2 (MonoidMap.get k1 m)

set :: (Ord k1, Ord k2, MonoidNull v)
    => k1
    -> k2
    -> v
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
set k1 k2 v (NestedMonoidMap m) =
    NestedMonoidMap $ MonoidMap.adjust (MonoidMap.set k2 v) k1 m

adjust
    :: (Ord k1, Ord k2, MonoidNull v)
    => (v -> v)
    -> k1
    -> k2
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
adjust f k1 k2 (NestedMonoidMap m) =
    NestedMonoidMap $ MonoidMap.adjust (MonoidMap.adjust f k2) k1 m

nullify
    :: (Ord k1, Ord k2, MonoidNull v)
    => k1
    -> k2
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
nullify k1 k2 (NestedMonoidMap m) =
    NestedMonoidMap $ MonoidMap.adjust (MonoidMap.nullify k2) k1 m

--------------------------------------------------------------------------------
-- Membership
--------------------------------------------------------------------------------

nonNullCount :: NestedMonoidMap k1 k2 v -> Int
nonNullCount (NestedMonoidMap m) =
    getSum $ F.foldMap (Sum . MonoidMap.nonNullCount) m

nonNullKey
    :: (Ord k1, Ord k2, MonoidNull v)
    => k1
    -> k2
    -> NestedMonoidMap k1 k2 v
    -> Bool
nonNullKey k1 k2 (NestedMonoidMap m) =
    MonoidMap.nonNullKey k2 (MonoidMap.get k1 m)

nonNullKeys
    :: (Ord k1, Ord k2, MonoidNull v)
    => NestedMonoidMap k1 k2 v
    -> Set (k1, k2)
nonNullKeys = Set.fromList . fmap fst . toFlatList

--------------------------------------------------------------------------------
-- Intersection
--------------------------------------------------------------------------------

intersection
    :: (Ord k1, Ord k2, MonoidNull v, GCDMonoid v)
    => NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
intersection (NestedMonoidMap m1) (NestedMonoidMap m2) = NestedMonoidMap $
    MonoidMap.intersection m1 m2

intersectionWith
    :: (Ord k1, Ord k2, MonoidNull v)
    => (v -> v -> v)
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
intersectionWith f (NestedMonoidMap m1) (NestedMonoidMap m2) = NestedMonoidMap $
    MonoidMap.intersectionWith (MonoidMap.intersectionWith f) m1 m2

--------------------------------------------------------------------------------
-- Union
--------------------------------------------------------------------------------

union
    :: (Ord k1, Ord k2, MonoidNull v, LCMMonoid v)
    => NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
union (NestedMonoidMap m1) (NestedMonoidMap m2) = NestedMonoidMap $
    MonoidMap.union m1 m2

unionWith
    :: (Ord k1, Ord k2, MonoidNull v)
    => (v -> v -> v)
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
unionWith f (NestedMonoidMap m1) (NestedMonoidMap m2) = NestedMonoidMap $
    MonoidMap.unionWith (MonoidMap.unionWith f) m1 m2

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

isSubmapOf
    :: (Ord k1, Ord k2, MonoidNull v, Reductive v)
    => NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
    -> Bool
isSubmapOf (NestedMonoidMap m1) (NestedMonoidMap m2) =
    MonoidMap.isSubmapOf m1 m2

isSubmapOfBy
    :: (Ord k1, Ord k2, MonoidNull v, Reductive v)
    => (v -> v -> Bool)
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
    -> Bool
isSubmapOfBy f (NestedMonoidMap m1) (NestedMonoidMap m2) =
    MonoidMap.isSubmapOfBy (MonoidMap.isSubmapOfBy f) m1 m2

disjoint
    :: (Ord k1, Ord k2, MonoidNull v, GCDMonoid v)
    => NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
    -> Bool
disjoint (NestedMonoidMap m1) (NestedMonoidMap m2) =
    MonoidMap.disjoint m1 m2

disjointBy
    :: (Ord k1, Ord k2, MonoidNull v, GCDMonoid v)
    => (v -> v -> Bool)
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
    -> Bool
disjointBy f (NestedMonoidMap m1) (NestedMonoidMap m2) =
    MonoidMap.disjointBy (MonoidMap.disjointBy f) m1 m2
