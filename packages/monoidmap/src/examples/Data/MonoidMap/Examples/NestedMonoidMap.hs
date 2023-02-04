{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
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
    , nonNullKeys
    )
    where

import Prelude

import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, OverlappingGCDMonoid, RightGCDMonoid )
import Data.Monoid.Monus
    ( Monus )
import Data.Monoid.Null
    ( MonoidNull, PositiveMonoid )
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
import Data.Total.MonoidMap
    ( MonoidMap )
import GHC.Exts
    ( IsList (..) )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Total.MonoidMap as MonoidMap

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
    acc m (k, v) = adjust (<> v) k m

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
    => (k1, k2)
    -> NestedMonoidMap k1 k2 v
    -> v
get (k1, k2) (NestedMonoidMap m) = MonoidMap.get k2 (MonoidMap.get k1 m)

set :: (Ord k1, Ord k2, MonoidNull v)
    => (k1, k2)
    -> v
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
set (k1, k2) v (NestedMonoidMap m) = NestedMonoidMap $
    MonoidMap.set k1 (MonoidMap.set k2 v (MonoidMap.get k1 m)) m

adjust
    :: (Ord k1, Ord k2, MonoidNull v)
    => (v -> v)
    -> (k1, k2)
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
adjust f k m = set k (f $ get k m) m

nullify
    :: (Ord k1, Ord k2, MonoidNull v)
    => (k1, k2)
    -> NestedMonoidMap k1 k2 v
    -> NestedMonoidMap k1 k2 v
nullify k = set k mempty

--------------------------------------------------------------------------------
-- Membership
--------------------------------------------------------------------------------

nonNullCount :: NestedMonoidMap k1 k2 v -> Int
nonNullCount (NestedMonoidMap m) =
    getSum $ F.foldMap (Sum . MonoidMap.nonNullCount) m

nonNullKeys
    :: (Ord k1, Ord k2, MonoidNull v)
    => NestedMonoidMap k1 k2 v
    -> Set (k1, k2)
nonNullKeys = Set.fromList . fmap fst . toFlatList
