{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

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
    , delete

    -- * Queries
    , keys
    , member
    , notMember
    , null
    , notNull
    , size

    -- * Slicing
    , take
    , drop
    , splitAt

    -- * Filtering
    , filter
    , filterKeys
    , filterValues

    -- * Partitioning
    , partition
    , partitionKeys
    , partitionValues

    -- * Traversal
    , map

    -- * Combination
    , intersectionWith
    , intersectionWithF
    , unionWith
    , unionWithF
    )
    where

import Prelude hiding
    ( drop, filter, gcd, lookup, map, null, splitAt, subtract, take )

import Control.DeepSeq
    ( NFData )
import Data.Bifoldable
    ( Bifoldable )
import Data.Bifunctor
    ( bimap )
import Data.Functor.Classes
    ( Eq1, Eq2, Show1, Show2 )
import Data.Group
    ( Group (..) )
import Data.Maybe
    ( fromMaybe )
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
import Data.Strict.Map
    ( Map )
import Data.Strict.Map.Autogen.Merge.Strict
    ( dropMissing
    , mapMaybeMissing
    , traverseMaybeMissing
    , zipWithMaybeAMatched
    , zipWithMaybeMatched
    )
import Data.Strict.Set
    ( Set )
import GHC.Exts
    ( IsList (Item) )
import Text.Read
    ( Read (..) )

import qualified Data.Foldable as F
import qualified Data.Monoid.Null as Null
import qualified Data.Strict.Map as Map
import qualified Data.Strict.Map.Autogen.Merge.Strict as Map
import qualified GHC.Exts as GHC

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype MonoidMap k v = MonoidMap
    { unMonoidMap :: Map k v }
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
    (<>) = unionWith (<>)

instance (Ord k, MonoidNull v, Group v) => Group (MonoidMap k v)
  where
    invert = map invert
    (~~) = unionWith (~~)
    m `pow` x = map (`pow` x) m

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | The empty 'MonoidMap'.
--
empty :: MonoidMap k v
empty = MonoidMap Map.empty

-- | Constructs a 'MonoidMap' from a list of key-value pairs.
--
-- If the list contains more than one value for the same key, values are
-- combined together with '<>'.
--
fromList :: (Ord k, MonoidNull v) => [(k, v)] -> MonoidMap k v
fromList = fromListWith (<>)

-- | Constructs a 'MonoidMap' from a list of key-value pairs.
--
-- If the list contains more than one value for the same key, values are
-- combined together with the given combination function.
--
fromListWith
    :: (Ord k, MonoidNull v)
    => (v -> v -> v)
    -- ^ Combination function with which to combine values for duplicate keys.
    -> [(k, v)]
    -> MonoidMap k v
fromListWith f kvs = adjustMany f kvs mempty

-- | Constructs a 'MonoidMap' from an ordinary 'Map'.
--
fromMap :: MonoidNull v => Map k v -> MonoidMap k v
fromMap = MonoidMap . Map.filter (not . Null.null)

-- | Constructs a 'MonoidMap' from a single key-value pair.
--
singleton :: (Ord k, MonoidNull v) => k -> v -> MonoidMap k v
singleton k v = set k v mempty

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

-- | Converts a 'MonoidMap' to a list of key-value pairs.
--
-- The result only includes entries with values that are not 'null'.
--
toList :: MonoidMap k v -> [(k, v)]
toList = Map.toList . unMonoidMap

-- | Converts a 'MonoidMap' to a 'Map'.
--
-- The result only includes entries with values that are not 'null'.
--
toMap :: MonoidMap k v -> Map k v
toMap = unMonoidMap

--------------------------------------------------------------------------------
-- Basic operations
--------------------------------------------------------------------------------

-- | Gets the value associated with the given key.
--
get :: (Ord k, Monoid v) => k -> MonoidMap k v -> v
get k m = fromMaybe mempty $ Map.lookup k $ toMap m

-- | Sets the value associated with the given key.
--
set :: (Ord k, MonoidNull v) => k -> v -> MonoidMap k v -> MonoidMap k v
set k v m
    | Null.null v = MonoidMap $ Map.delete k   $ unMonoidMap m
    | otherwise   = MonoidMap $ Map.insert k v $ unMonoidMap m

-- | Adjusts the value associated with the given key.
--
adjust
    :: (Ord k, MonoidNull v)
    => (v -> v)
    -> k
    -> MonoidMap k v
    -> MonoidMap k v
adjust f k m = set k (f (get k m)) m

adjustMany
    :: (Ord k, MonoidNull v, IsList kvs, Item kvs ~ (k, v))
    => (v -> v -> v)
    -> kvs
    -> MonoidMap k v
    -> MonoidMap k v
adjustMany f kvs m0 =
    F.foldl' acc m0 (GHC.toList kvs)
  where
    acc m (k, v) = adjust (f v) k m

-- | Sets the value associated with the given key to 'mempty'.
--
delete :: (Ord k, MonoidNull v) => k -> MonoidMap k v -> MonoidMap k v
delete k = set k mempty

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | Returns the set of keys associated with values that are not 'null'.
--
keys :: MonoidMap k v -> Set k
keys = Map.keysSet . toMap

-- | Returns 'True' if (and only if) the given key is associated with a value
--   that is not 'null'.
--
member :: Ord k => k -> MonoidMap k v -> Bool
member k = Map.member k . toMap

-- | Returns 'True' if (and only if) the given key is associated with a value
--   that is 'null'.
--
notMember :: Ord k => k -> MonoidMap k v -> Bool
notMember k = Map.notMember k . toMap

-- | Returns 'True' if (and only if) all values in the map are 'null'.
--
null :: MonoidMap k v -> Bool
null = Map.null . toMap

-- | Returns 'True' if (and only if) the map contains at least one value that
--   is not 'null'.
--
notNull :: MonoidMap k v -> Bool
notNull = not . null

-- | Returns a count of all values in the map that are not 'null'.
--
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
-- Slicing
--------------------------------------------------------------------------------

-- | Takes a given number of entries in key order, beginning with the smallest
--   keys.
--
-- @
-- 'take' n == 'fromList' . 'Prelude.take' n . 'toList'
-- @
--
take :: Int -> MonoidMap k v -> MonoidMap k v
take i (MonoidMap m) = MonoidMap (Map.take i m)

-- | Drops a given number of entries in key order, beginning with the smallest
--   keys.
--
-- @
-- 'drop' n == 'fromList' . 'Prelude.drop' n . 'toList'
-- @
--
drop :: Int -> MonoidMap k v -> MonoidMap k v
drop i (MonoidMap m) = MonoidMap (Map.drop i m)

-- | Splits a map at a particular index.
--
-- @
-- 'splitAt' n xs == ('take' n xs, 'drop' n xs)
-- @
--
splitAt :: Int -> MonoidMap k a -> (MonoidMap k a, MonoidMap k a)
splitAt i m = (take i m, drop i m)

--------------------------------------------------------------------------------
-- Filtering
--------------------------------------------------------------------------------

-- | Filters a map according to a predicate on keys and values.
--
-- The result contains just the subset of entries that satisfy the predicate.
--
-- @
-- 'toList' ('filter' f m) == 'List.filter' ('uncurry' f) ('toList' m)
-- @
--
filter :: (k -> v -> Bool) -> MonoidMap k v -> MonoidMap k v
filter f (MonoidMap m) = MonoidMap $ Map.filterWithKey f m

-- | Filters a map according to a predicate on keys.
--
-- The result contains just the subset of entries that satisfy the predicate.
--
-- @
-- 'filterKeys' f m == 'filter' (\\k _ -> f k) m
-- @
--
filterKeys :: (k -> Bool) -> MonoidMap k v -> MonoidMap k v
filterKeys f (MonoidMap m) = MonoidMap $ Map.filterWithKey (\k _ -> f k) m

-- | Filters a map according to a predicate on values.
--
-- The result contains just the subset of entries that satisfy the predicate.
--
-- @
-- 'filterValues' f m == 'filter' (\\_ v -> f v) m
-- @
--
filterValues :: (v -> Bool) -> MonoidMap k v -> MonoidMap k v
filterValues f (MonoidMap m) = MonoidMap $ Map.filter f m

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

-- | Partitions a map according to a predicate on keys and values.
--
-- The first map contains all entries that satisfy the predicate, and the
-- second map contains all entries that fail the predicate.
--
-- @
-- 'partition' f m == ('filter' f m, 'filter' (\k v -> 'not' (f k v)) m)
-- @
--
partition
    :: (k -> v -> Bool) -> MonoidMap k v -> (MonoidMap k v, MonoidMap k v)
partition f (MonoidMap m) =
    bimap MonoidMap MonoidMap $ Map.partitionWithKey f m

-- | Partitions a map according to a predicate on keys.
--
-- The first map contains all entries that satisfy the predicate, and the
-- second map contains all entries that fail the predicate.
--
-- @
-- 'partitionKeys' f m == ('filterKeys' f m, 'filterKeys' ('not' . f) m)
-- @
--
partitionKeys
    :: (k -> Bool) -> MonoidMap k v -> (MonoidMap k v, MonoidMap k v)
partitionKeys f (MonoidMap m) =
    bimap MonoidMap MonoidMap $ Map.partitionWithKey (\k _ -> f k) m

-- | Partitions a map according to a predicate on values.
--
-- The first map contains all entries that satisfy the predicate, and the
-- second map contains all entries that fail the predicate.
--
-- @
-- 'partitionValues' f m == ('filterValues' f m, 'filterValues' ('not' . f) m)
-- @
--
partitionValues
    :: (v -> Bool) -> MonoidMap k v -> (MonoidMap k v, MonoidMap k v)
partitionValues f (MonoidMap m) =
    bimap MonoidMap MonoidMap $ Map.partition f m

--------------------------------------------------------------------------------
-- Traversal
--------------------------------------------------------------------------------

-- | Applies the given function to all values in the map that are not 'null'.
--
-- If the range of the given function includes 'mempty', then the resultant map
-- may have a 'size' that is smaller than the original map.
--
map
    :: MonoidNull v2
    => (v1 -> v2)
    -> MonoidMap k v1
    -> MonoidMap k v2
map f (MonoidMap m) = MonoidMap $ Map.mapMaybe (guardNotNull . f) m

--------------------------------------------------------------------------------
-- Binary operations
--------------------------------------------------------------------------------

intersectionWith
    :: (Ord k, MonoidNull v3)
    => (v1 -> v2 -> v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> MonoidMap k v3
intersectionWith f (MonoidMap m1) (MonoidMap m2) = MonoidMap $ Map.merge
    dropMissing
    dropMissing
    (zipWithMaybeMatched $ \_ v1 v2 -> guardNotNull $ f v1 v2)
    m1 m2

intersectionWithF
    :: (Applicative f, Ord k, MonoidNull v3)
    => (v1 -> v2 -> f v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> f (MonoidMap k v3)
intersectionWithF f (MonoidMap m1) (MonoidMap m2) = MonoidMap <$> Map.mergeA
    dropMissing
    dropMissing
    (zipWithMaybeAMatched $ \_ v1 v2 -> guardNotNull <$> f v1 v2)
    m1 m2

unionWith
    :: (Ord k, Monoid v1, Monoid v2, MonoidNull v3)
    => (v1 -> v2 -> v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> MonoidMap k v3
unionWith f (MonoidMap m1) (MonoidMap m2) = MonoidMap $ Map.merge
    (mapMaybeMissing $ \_ v1 -> guardNotNull $ f v1 mempty)
    (mapMaybeMissing $ \_ v2 -> guardNotNull $ f mempty v2)
    (zipWithMaybeMatched $ \_ v1 v2 -> guardNotNull $ f v1 v2)
    m1 m2

unionWithF
    :: (Applicative f, Ord k, Monoid v1, Monoid v2, MonoidNull v3)
    => (v1 -> v2 -> f v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> f (MonoidMap k v3)
unionWithF f (MonoidMap m1) (MonoidMap m2) = MonoidMap <$> Map.mergeA
    (traverseMaybeMissing $ \_ v1 -> guardNotNull <$> f v1 mempty)
    (traverseMaybeMissing $ \_ v2 -> guardNotNull <$> f mempty v2)
    (zipWithMaybeAMatched $ \_ v1 v2 -> guardNotNull <$> f v1 v2)
    m1 m2

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

guardNotNull :: MonoidNull v => v -> Maybe v
guardNotNull v
    | Null.null v = Nothing
    | otherwise   = Just v
{-# INLINE guardNotNull #-}
