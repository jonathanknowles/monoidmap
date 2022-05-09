-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal
    (
    -- * Type
      MonoidMap

    -- * Construction
    , empty
    , fromList
    , fromListWith
    , fromMap
    , singleton

    -- * Deconstruction
    , toList
    , toMap

    -- * Queries
    , keysSet
    , lookup
    , null
    , member
    , size

    -- * Modification
    , adjust
    , delete
    , insert
    , insertWith

    -- * Traversal
    , map

    -- * Combination
    , mergeWith
    , mergeWithF
    , intersectionWith
    , intersectionWithF
    , unionWith
    , unionWithF
    )
    where

import Prelude hiding
    ( gcd, lookup, map, null, subtract )

import Data.Functor.Identity
    ( Identity (..) )
import Data.Map.Strict
    ( Map )
import Data.Monoid.GCD
    ( GCDMonoid (..)
    , LeftGCDMonoid (..)
    , OverlappingGCDMonoid (..)
    , RightGCDMonoid (..)
    )
import Data.Monoid.Monus
    ( Monus (..) )
import Data.Monoid.Null
    ( MonoidNull, PositiveMonoid )
import Data.Semigroup.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive (..)
    , Reductive (..)
    , RightCancellative
    , RightReductive (..)
    )
import Data.Set
    ( Set )
import GHC.Exts
    ( IsList (Item) )
import Text.Read
    ( Read (..) )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Monoid.Null as Null
import qualified Data.MonoidMap.Internal.Core as Core
import qualified Data.Set as Set
import qualified GHC.Exts as GHC

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype MonoidMap k v = MonoidMap
    { unMonoidMap :: Core.MonoidMap k v }
    deriving (Eq, Foldable)
    deriving newtype Show

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance (Ord k, Read k, Eq v, Monoid v, Read v) =>
    Read (MonoidMap k v)
  where
    readPrec = fromMap <$> readPrec

instance (Ord k, Eq v, Monoid v) =>
    MonoidNull (MonoidMap k v)
  where
    null = null

instance (Ord k, Eq v, PositiveMonoid v) =>
    PositiveMonoid (MonoidMap k v)

instance (Ord k, Eq v, Monoid v, Commutative v) =>
    Commutative (MonoidMap k v)

instance (Ord k, Eq v, Monoid v, LeftReductive v) =>
    LeftReductive (MonoidMap k v)
  where
    isPrefixOf = isSubmapOfBy isPrefixOf
    stripPrefix = unionWithF stripPrefix

instance (Ord k, Eq v, Monoid v, RightReductive v) =>
    RightReductive (MonoidMap k v)
  where
    isSuffixOf = isSubmapOfBy isSuffixOf
    stripSuffix = unionWithF stripSuffix

instance (Ord k, Eq v, Monoid v, Reductive v) =>
    Reductive (MonoidMap k v)
  where
    (</>) = unionWithF (</>)

instance (Ord k, Eq v, Monoid v, LeftCancellative v) =>
    LeftCancellative (MonoidMap k v)

instance (Ord k, Eq v, Monoid v, RightCancellative v) =>
    RightCancellative (MonoidMap k v)

instance (Ord k, Eq v, Monoid v, Cancellative v) =>
    Cancellative (MonoidMap k v)

instance (Ord k, Eq v, Monoid v, GCDMonoid v) =>
    GCDMonoid (MonoidMap k v)
  where
    gcd = intersectionWith gcd

instance (Ord k, Eq v, Monoid v, LeftGCDMonoid v) =>
    LeftGCDMonoid (MonoidMap k v)
  where
    commonPrefix = intersectionWith commonPrefix

instance (Ord k, Eq v, Monoid v, RightGCDMonoid v) =>
    RightGCDMonoid (MonoidMap k v)
  where
    commonSuffix = intersectionWith commonSuffix

instance (Ord k, Eq v, Monoid v, OverlappingGCDMonoid v) =>
    OverlappingGCDMonoid (MonoidMap k v)
  where
    overlap = intersectionWith overlap
    stripPrefixOverlap = unionWith stripPrefixOverlap
    stripSuffixOverlap = unionWith stripSuffixOverlap
    stripOverlap m1 m2 =
        ( stripSuffixOverlap m2 m1
        , m1 `overlap` m2
        , stripPrefixOverlap m1 m2
        )

instance (Ord k, Eq v, Monoid v, Monus v) =>
    Monus (MonoidMap k v)
  where
    (<\>) = unionWith (<\>)

instance (Ord k, Eq v, Monoid v) => IsList (MonoidMap k v)
  where
    type Item (MonoidMap k v) = (k, v)
    fromList = fromList
    toList = toList

instance (Ord k, Eq v, Monoid v) => Monoid (MonoidMap k v)
  where
    mempty = empty

instance (Ord k, Eq v, Monoid v) => Semigroup (MonoidMap k v)
  where
    (<>) = adjustMany (flip (<>))

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

empty :: MonoidMap k v
empty = MonoidMap Core.empty

fromList :: (Ord k, Eq v, Monoid v) => [(k, v)] -> MonoidMap k v
fromList = fromListWith (<>)

fromListWith
    :: (Ord k, Eq v, Monoid v)
    => (v -> v -> v)
    -> [(k, v)]
    -> MonoidMap k v
fromListWith f = adjustMany f mempty

fromMap :: (Ord k, Eq v, Monoid v) => Map k v -> MonoidMap k v
fromMap = fromList . Map.toList

singleton :: (Ord k, Eq v, Monoid v) => k -> v -> MonoidMap k v
singleton = set mempty

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

toList :: MonoidMap k v -> [(k, v)]
toList = Map.toList . Core.toMap . unMonoidMap

toMap :: MonoidMap k v -> Map k v
toMap = Core.toMap . unMonoidMap

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

lookup :: (Ord k, Monoid v) => k -> MonoidMap k v -> v
lookup = flip get

member :: Ord k => k -> MonoidMap k a -> Bool
member k = Map.member k . toMap

null :: MonoidMap k a -> Bool
null = Map.null . toMap

get :: (Ord k, Monoid v) => MonoidMap k v -> k -> v
get = Core.get . unMonoidMap

keysSet :: MonoidMap k v -> Set k
keysSet = Map.keysSet . toMap

size :: MonoidMap k v -> Int
size = Map.size . toMap

isSubmapOfBy
    :: Ord k
    => (v1 -> v2 -> Bool)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> Bool
isSubmapOfBy f m1 m2 = Map.isSubmapOfBy f (toMap m1) (toMap m2)

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

adjust
    :: (Ord k, Eq v, Monoid v)
    => (v -> v)
    -> k
    -> MonoidMap k v
    -> MonoidMap k v
adjust f k m = set m k $ f (get m k)

adjustMany
    :: (Ord k, Eq v, Monoid v, IsList many, Item many ~ (k, v))
    => (v -> v -> v)
    -> MonoidMap k v
    -> many
    -> MonoidMap k v
adjustMany f m1 m2 =
    F.foldl' acc m1 (GHC.toList m2)
  where
    acc m (k, v) = adjust (f v) k m

delete :: (Ord k, Eq v, Monoid v) => k -> MonoidMap k v -> MonoidMap k v
delete k m = set m k mempty

insert :: (Ord k, Eq v, Monoid v) => k -> v -> MonoidMap k v -> MonoidMap k v
insert = insertWith const

insertWith
    :: (Ord k, Eq v, Monoid v)
    => (v -> v -> v)
    -> k
    -> v
    -> MonoidMap k v
    -> MonoidMap k v
insertWith f k v m = set m k $ f v (get m k)

set :: (Ord k, Eq v, Monoid v) => MonoidMap k v -> k -> v -> MonoidMap k v
set = ((MonoidMap .) .) . Core.set . unMonoidMap

--------------------------------------------------------------------------------
-- Traversal
--------------------------------------------------------------------------------

map
    :: (Ord k, Eq v2, Monoid v2)
    => (v1 -> v2)
    -> MonoidMap k v1
    -> MonoidMap k v2
map f = fromList . fmap (fmap f) . toList

--------------------------------------------------------------------------------
-- Binary operations
--------------------------------------------------------------------------------

mergeWith
    :: forall k v. (Ord k, Eq v, Monoid v)
    => (Set k -> Set k -> Set k)
    -> (v -> v -> v)
    -> MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
mergeWith mergeKeys mergeValue m1 m2 =
    runIdentity $ mergeWithF mergeKeys (fmap (fmap Identity) mergeValue) m1 m2

mergeWithF
    :: forall f k v. (Applicative f, Ord k, Eq v, Monoid v)
    => (Set k -> Set k -> Set k)
    -> (v -> v -> f v)
    -> MonoidMap k v
    -> MonoidMap k v
    -> f (MonoidMap k v)
mergeWithF mergeKeys mergeValue m1 m2
    = fmap fromList
    $ traverse merge
    $ F.toList
    $ mergeKeys (keysSet m1) (keysSet m2)
  where
    merge :: k -> f (k, v)
    merge k = (k,) <$> mergeValue (m1 `get` k) (m2 `get` k)

intersectionWith
    :: forall k v. (Ord k, Eq v, Monoid v)
    => (v -> v -> v)
    -> MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
intersectionWith = mergeWith Set.intersection

intersectionWithF
    :: forall f k v. (Applicative f, Ord k, Eq v, Monoid v)
    => (v -> v -> f v)
    -> MonoidMap k v
    -> MonoidMap k v
    -> f (MonoidMap k v)
intersectionWithF = mergeWithF Set.intersection

unionWith
    :: forall k v. (Ord k, Eq v, Monoid v)
    => (v -> v -> v)
    -> MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
unionWith = mergeWith Set.union

unionWithF
    :: forall f k v. (Applicative f, Ord k, Eq v, Monoid v)
    => (v -> v -> f v)
    -> MonoidMap k v
    -> MonoidMap k v
    -> f (MonoidMap k v)
unionWithF = mergeWithF Set.union
