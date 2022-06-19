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

    -- * Basic operations
    , get
    , set
    , adjust
    , nullify

    -- * Queries
    , null
    , nullKey
    , nonNull
    , nonNullKey
    , nonNullKeys
    , size

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

import Control.DeepSeq
    ( NFData )
import Data.Bifoldable
    ( Bifoldable )
import Data.Functor.Classes
    ( Eq1, Eq2, Show1, Show2 )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Group
    ( Group (..) )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( All (..) )
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
    deriving newtype
        (Bifoldable, Eq, Eq1, Eq2, Foldable, NFData, Show, Show1, Show2)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance (Ord k, Read k, MonoidNull v, Read v) =>
    Read (MonoidMap k v)
  where
    readPrec = fromMap <$> readPrec

instance (Ord k, MonoidNull v) =>
    MonoidNull (MonoidMap k v)
  where
    null = null

instance (Ord k, PositiveMonoid v) =>
    PositiveMonoid (MonoidMap k v)

instance (Ord k, MonoidNull v, Commutative v) =>
    Commutative (MonoidMap k v)

instance (Ord k, MonoidNull v, LeftReductive v) =>
    LeftReductive (MonoidMap k v)
  where
    isPrefixOf = isSubmapOfBy isPrefixOf
    stripPrefix = unionWithF stripPrefix

instance (Ord k, MonoidNull v, RightReductive v) =>
    RightReductive (MonoidMap k v)
  where
    isSuffixOf = isSubmapOfBy isSuffixOf
    stripSuffix = unionWithF stripSuffix

instance (Ord k, MonoidNull v, Reductive v) =>
    Reductive (MonoidMap k v)
  where
    (</>) = unionWithF (</>)

instance (Ord k, MonoidNull v, LeftCancellative v) =>
    LeftCancellative (MonoidMap k v)

instance (Ord k, MonoidNull v, RightCancellative v) =>
    RightCancellative (MonoidMap k v)

instance (Ord k, MonoidNull v, Cancellative v) =>
    Cancellative (MonoidMap k v)

instance (Ord k, MonoidNull v, GCDMonoid v) =>
    GCDMonoid (MonoidMap k v)
  where
    gcd = intersectionWith gcd

instance (Ord k, MonoidNull v, LeftGCDMonoid v) =>
    LeftGCDMonoid (MonoidMap k v)
  where
    commonPrefix = intersectionWith commonPrefix

instance (Ord k, MonoidNull v, RightGCDMonoid v) =>
    RightGCDMonoid (MonoidMap k v)
  where
    commonSuffix = intersectionWith commonSuffix

instance (Ord k, MonoidNull v, OverlappingGCDMonoid v) =>
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

instance (Ord k, MonoidNull v, Monus v) =>
    Monus (MonoidMap k v)
  where
    (<\>) = unionWith (<\>)

instance (Ord k, MonoidNull v) => IsList (MonoidMap k v)
  where
    type Item (MonoidMap k v) = (k, v)
    fromList = fromList
    toList = toList

instance (Ord k, MonoidNull v) => Monoid (MonoidMap k v)
  where
    mempty = empty

instance (Ord k, MonoidNull v) => Semigroup (MonoidMap k v)
  where
    (<>) = adjustMany (flip (<>))

instance (Ord k, MonoidNull v, Group v) => Group (MonoidMap k v)
  where
    invert = map invert
    (~~) = unionWith (~~)
    m `pow` x = map (`pow` x) m

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

empty :: MonoidMap k v
empty = MonoidMap Core.empty

fromList :: (Ord k, MonoidNull v) => [(k, v)] -> MonoidMap k v
fromList = fromListWith (<>)

fromListWith
    :: (Ord k, MonoidNull v)
    => (v -> v -> v)
    -> [(k, v)]
    -> MonoidMap k v
fromListWith f = adjustMany f mempty

fromMap :: (Ord k, MonoidNull v) => Map k v -> MonoidMap k v
fromMap = fromList . Map.toList

singleton :: (Ord k, MonoidNull v) => k -> v -> MonoidMap k v
singleton k v = set k v mempty

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

toList :: MonoidMap k v -> [(k, v)]
toList = Map.toList . Core.toMap . unMonoidMap

toMap :: MonoidMap k v -> Map k v
toMap = Core.toMap . unMonoidMap

--------------------------------------------------------------------------------
-- Basic operations
--------------------------------------------------------------------------------

get :: (Ord k, Monoid v) => k -> MonoidMap k v -> v
get k = Core.get k . unMonoidMap

set :: (Ord k, MonoidNull v) => k -> v -> MonoidMap k v -> MonoidMap k v
set k v = MonoidMap . Core.set k v . unMonoidMap

adjust
    :: (Ord k, MonoidNull v)
    => (v -> v)
    -> k
    -> MonoidMap k v
    -> MonoidMap k v
adjust f k m = set k (f (get k m)) m

adjustMany
    :: (Ord k, MonoidNull v, IsList many, Item many ~ (k, v))
    => (v -> v -> v)
    -> MonoidMap k v
    -> many
    -> MonoidMap k v
adjustMany f m1 m2 =
    F.foldl' acc m1 (GHC.toList m2)
  where
    acc m (k, v) = adjust (f v) k m

nullify :: (Ord k, MonoidNull v) => MonoidMap k v -> k -> MonoidMap k v
nullify m k = set k mempty m

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

null :: MonoidMap k v -> Bool
null = Map.null . toMap

nullKey :: Ord k => MonoidMap k v -> k -> Bool
nullKey m k = Map.notMember k (toMap m)

nonNull :: MonoidMap k v -> Bool
nonNull = not . null

nonNullKey :: Ord k => MonoidMap k v -> k -> Bool
nonNullKey m k = Map.member k (toMap m)

nonNullKeys :: MonoidMap k v -> Set k
nonNullKeys = Map.keysSet . toMap

size :: MonoidMap k v -> Int
size = Map.size . toMap

isSubmapOfBy
    :: (Ord k, Monoid v1, Monoid v2)
    => (v1 -> v2 -> Bool)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> Bool
isSubmapOfBy f m1 m2 = getAll $ F.fold $ unionWith (fmap (fmap All) f) m1 m2

--------------------------------------------------------------------------------
-- Traversal
--------------------------------------------------------------------------------

map
    :: (Ord k, MonoidNull v2)
    => (v1 -> v2)
    -> MonoidMap k v1
    -> MonoidMap k v2
map f = fromList . fmap (fmap f) . toList

--------------------------------------------------------------------------------
-- Binary operations
--------------------------------------------------------------------------------

mergeWith
    :: forall k v1 v2 v3.
        (Ord k, Monoid v1, Monoid v2, MonoidNull v3)
    => (Set k -> Set k -> Set k)
    -> (v1 -> v2 -> v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> MonoidMap k v3
mergeWith mergeKeys mergeValue m1 m2 =
    runIdentity $ mergeWithF mergeKeys (fmap (fmap Identity) mergeValue) m1 m2

mergeWithF
    :: forall f k v1 v2 v3.
        (Applicative f, Ord k, Monoid v1, Monoid v2, MonoidNull v3)
    => (Set k -> Set k -> Set k)
    -> (v1 -> v2 -> f v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> f (MonoidMap k v3)
mergeWithF mergeKeys mergeValue m1 m2
    = fmap fromList
    $ traverse merge
    $ F.toList
    $ mergeKeys (nonNullKeys m1) (nonNullKeys m2)
  where
    merge :: k -> f (k, v3)
    merge k = (k,) <$> mergeValue (get k m1) (get k m2)

intersectionWith
    :: forall k v1 v2 v3.
        (Ord k, Monoid v1, Monoid v2, MonoidNull v3)
    => (v1 -> v2 -> v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> MonoidMap k v3
intersectionWith = mergeWith Set.intersection

intersectionWithF
    :: forall f k v1 v2 v3.
        (Applicative f, Ord k, Monoid v1, Monoid v2, MonoidNull v3)
    => (v1 -> v2 -> f v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> f (MonoidMap k v3)
intersectionWithF = mergeWithF Set.intersection

unionWith
    :: forall k v1 v2 v3.
        (Ord k, Monoid v1, Monoid v2, MonoidNull v3)
    => (v1 -> v2 -> v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> MonoidMap k v3
unionWith = mergeWith Set.union

unionWithF
    :: forall f k v1 v2 v3.
        (Applicative f, Ord k, Monoid v1, Monoid v2, MonoidNull v3)
    => (v1 -> v2 -> f v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> f (MonoidMap k v3)
unionWithF = mergeWithF Set.union
