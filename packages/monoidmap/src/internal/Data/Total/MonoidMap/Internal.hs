{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides /internal/ operations for the 'MonoidMap' type.
--
module Data.Total.MonoidMap.Internal
    (
    -- * Type
      MonoidMap (..)

    -- * Construction
    , empty
    , fromList
    , fromListWith
    , fromMap
    , singleton

    -- * Deconstruction
    , toList
    , toMap

    -- * Lookup
    , get

    -- * Modification
    , set
    , adjust
    , nullify

    -- * Membership
    , null
    , nullKey
    , nonNull
    , nonNullCount
    , nonNullKey
    , nonNullKeys

    -- * Slicing
    , take
    , drop
    , splitAt

    -- * Filtering
    , filter
    , filterKeys
    , filterWithKey

    -- * Partitioning
    , partition
    , partitionKeys
    , partitionWithKey

    -- * Mapping
    , map
    , mapKeys
    , mapKeysWith

    -- * Association
    , append

    -- * Subtraction
    , minus
    , minusMaybe
    , monus

    -- * Inversion
    , invert

    -- * Exponentiation
    , power

    -- * Prefixes
    , isPrefixOf
    , stripPrefix
    , commonPrefix
    , stripCommonPrefix

    -- * Suffixes
    , isSuffixOf
    , stripSuffix
    , commonSuffix
    , stripCommonSuffix

    -- * Overlap
    , overlap
    , stripPrefixOverlap
    , stripSuffixOverlap
    , stripOverlap

    -- * GCD
    , gcd

    -- * Combination
    , intersection
    , intersectionA
    , union
    , unionA
    )
    where

import Prelude hiding
    ( drop, filter, gcd, lookup, map, null, splitAt, subtract, take )

import Control.DeepSeq
    ( NFData )
import Data.Bifoldable
    ( Bifoldable )
import Data.Function
    ( (&) )
import Data.Functor.Classes
    ( Eq1, Eq2, Show1, Show2 )
import Data.Functor.Identity
    ( Identity )
import Data.Group
    ( Abelian, Group )
import Data.Map.Strict
    ( Map, lookup )
import Data.Maybe
    ( fromMaybe, isJust )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, OverlappingGCDMonoid, RightGCDMonoid )
import Data.Monoid.Monus
    ( Monus (..) )
import Data.Monoid.Null
    ( MonoidNull, PositiveMonoid )
import Data.Semigroup.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive
    , Reductive (..)
    , RightCancellative
    , RightReductive
    )
import Data.Set
    ( Set )
import GHC.Exts
    ( IsList (Item) )
import Text.Read
    ( Read (..) )

import qualified Data.Bifunctor as B
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Exts as GHC

import qualified Data.Group as C
import qualified Data.Monoid.GCD as C
import qualified Data.Monoid.Null as C
import qualified Data.Semigroup.Cancellative as C

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | Models a relation from unique keys to /monoidal/ values.
--
-- The mapping from keys to values is __total__: every possible key of type
-- __@k@__ is associated with a corresponding value of type __@v@__:
--
-- @
-- 'get' :: ('Ord' k, 'Monoid' v) => k -> 'MonoidMap' k v -> v
-- @
--
-- By default, every key in an 'empty' map is associated with a value of
-- 'mempty':
--
-- @
-- ∀ k. 'get' k 'empty' '==' 'mempty'
-- @
--
-- == Comparison with partial map types
--
-- The 'MonoidMap' type differs from the standard 'Map' type in how it relates
-- /keys/ to /values/:
--
--  - 'Map' __@k@__ __@v@__:
--
--      relates /keys/ of type __@k@__ to /values/ of type __'Maybe'__ __@v@__.
--
--  - 'MonoidMap' __@k@__ __@v@__:
--
--      relates /keys/ of type __@k@__ to /values/ of type __@v@__.
--
-- This becomes evident if we compare the type signatures of operations to
-- query a key for its value, for both types:
--
-- @
--       'Map'.'lookup' :: \      \      k ->       'Map' k v -> 'Maybe' v
-- 'MonoidMap'.'get'    :: 'Monoid' v => k -> 'MonoidMap' k v -> \     \ v
-- @
--
-- For /unconstrained/ value types, using 'Maybe' makes it possible to signal
-- the /presence/ or /absence/ of a value for a particular key.
--
-- However, /monoidal/ types have a natural way to represent empty values: the
-- 'mempty' constant, which represents the identity element of a monoid.
--
-- Consequently, using a standard 'Map' with a /monoidal/ value type gives rise
-- to /two/ distinct representations for missing or empty values:
--
-- +---------------------+--------------------------------------------+
-- | @                   |                                            |
-- | 'Map'.'lookup' k m  |                                            |
-- | @                   |                                            |
-- +=====================+============================================+
-- | @                   | Map __@m@__ has /no/ entry                 |
-- | 'Nothing'           | for key __@k@__.                           |
-- | @                   |                                            |
-- +---------------------+--------------------------------------------+
-- | @                   | Map __@m@__ /does/ have an entry           |
-- | 'Just' 'mempty'     | for key __@k@__, but the value is /empty/. |
-- | @                   |                                            |
-- +---------------------+--------------------------------------------+
--
-- In constrast, the 'MonoidMap' type provides a single, /canonical/
-- representation for empty values, according to the following mapping:
--
-- +------------------------------------+---+-------------------------+
-- | @                                  |   | @                       |
-- | 'Map'.'lookup' k m                 |   | 'MonoidMap'.'get' k m   |
-- | @                                  |   | @                       |
-- +====================================+===+=========================+
-- | @                                  |   | @                       |
-- | 'Nothing'                          | ⟼ | 'mempty'                |
-- | @                                  |   | @                       |
-- +--------------+---------------------+---+-------------------------+
-- | @            | @                   |   | @                       |
-- | 'Just' __v__ | __v__ '==' 'mempty' | ⟼ | 'mempty'                |
-- | @            | @                   |   | @                       |
-- +--------------+---------------------+---+-------------------------+
-- | @            | @                   |   | @                       |
-- | 'Just' __v__ | __v__ '/=' 'mempty' | ⟼ | __v__                   |
-- | @            | @                   |   | @                       |
-- +--------------+---------------------+---+-------------------------+
--
-- == Internal data structure
--
-- Internally, the 'MonoidMap' type uses a sparse 'Map' data structure to store
-- its key-value mappings, and only stores values that are /not/ equal to
-- 'mempty'.
--
-- Values that /are/ equal to 'mempty' are automatically garbage collected, and
-- /never/ included in the internal data structure.
--
-- As a result, the internal data structure is /always/ in a canonical form.
--
-- == Instances of 'Semigroup' and 'Monoid'
--
-- This module provides a 'Semigroup' instance that uses the '(<>)' operator to
-- combines values for matching keys, satisfying the following property:
--
-- @
-- 'get' k (m1 '<>' m2) == 'get' k m1 '<>' 'get' k m2
-- @
--
-- The 'Monoid' instance satisfies the following property for all possible keys:
--
-- @
-- 'get' k 'mempty' == 'mempty'
-- @
--
-- == Subclasses of 'Semigroup' and 'Monoid'
--
-- This module also provides instances for several __subclasses__ of
-- 'Semigroup' and 'Monoid'.
--
-- In general, these instances are defined /analogously/ to the 'Semigroup'
-- instance, where binary operations on /pairs/ /of/ /maps/ are defined in
-- terms of their application to /pairs/ /of/ /values/ for matching keys.
--
-- For example, if subclass __@C@__ defines a binary operation __@f@__ of the
-- form:
--
-- @
-- class 'Semigroup' a => C a where
--    f :: a -> a -> a
-- @
--
-- Then the result of applying __@f@__ to maps __@m1@__ and __@m2@__ will
-- typically satisfy a property of the following form:
--
-- @
-- ∀ k m1 m2. 'get' k (f m1 m2) == f ('get' k m1) ('get' k m2)
-- @
--
-- === __Examples__
--
-- The 'commonPrefix' function from 'LeftGCDMonoid':
--
-- @
-- 'get' k ('C.commonPrefix' m1 m2)
--     '==' 'C.commonPrefix' ('get' k m1) ('get' k m2)
-- @
--
-- The 'commonSuffix' function from 'RightGCDMonoid':
--
-- @
-- 'get' k ('C.commonSuffix' m1 m2)
--     '==' 'C.commonSuffix' ('get' k m1) ('get' k m2)
-- @
--
-- The 'overlap' function from 'OverlappingGCDMonoid':
--
-- @
-- 'get' k ('C.overlap' m1 m2)
--     '==' 'C.overlap' ('get' k m1) ('get' k m2)
-- @
--
-- == Constraints on value types
--
-- 'MonoidMap' operations generally require the value type to be an instance of
-- 'MonoidNull', which provides a convenient way to test whether or not a value
-- is 'mempty'.
--
-- Types that are instances of both 'MonoidNull' and 'Eq' satisfy the following
-- equivalence:
--
-- @
-- 'C.null' v '==' (v '==' 'mempty')
-- @
--
-- However, 'MonoidMap' operations generally do /not/ require that value types
-- are instances of 'Eq'.
--
-- === Justification
--
-- The set of monoidal types that admit a 'MonoidNull' instance is /strictly/
-- /larger/ than the set of monoidal types that admit an 'Eq' instance.
--
-- For any type __@v@__ that is an instance of both 'Eq' and 'Monoid', it is
-- /always/ possible to define a 'MonoidNull' instance:
--
-- @
-- instance 'MonoidNull' v where
--     'C.null' = ('==' 'mempty')
-- @
--
-- However, there are monoidal types for which it /is/ possible to define a
-- 'MonoidNull' instance, but /not/ practical (or possible) to define a lawful
-- 'Eq' instance.
--
-- For example, consider the following type:
--
-- @
-- 'Maybe' ('String' -> 'Data.Monoid.Sum' 'Numeric.Natural.Natural')
-- @
--
-- Requiring a 'MonoidNull' constraint instead of an 'Eq' constraint allows
-- 'MonoidMap' to be usable with a greater range of monoidal value types.
--
newtype MonoidMap k v = MonoidMap
    { unMonoidMap :: Map k v }
    deriving newtype
        (Eq, Eq1, Eq2, Foldable, Bifoldable, NFData, Show, Show1, Show2)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance (Ord k, MonoidNull v) =>
    IsList (MonoidMap k v)
  where
    type Item (MonoidMap k v) = (k, v)
    fromList = fromList
    toList = toList

instance (Ord k, Read k, MonoidNull v, Read v) =>
    Read (MonoidMap k v)
  where
    readPrec = fromMap <$> readPrec

--------------------------------------------------------------------------------
-- Instances: Semigroup and subclasses
--------------------------------------------------------------------------------

instance (Ord k, MonoidNull v) =>
    Semigroup (MonoidMap k v)
  where
    (<>) = append

instance (Ord k, MonoidNull v, Commutative v) =>
    Commutative (MonoidMap k v)

instance (Ord k, MonoidNull v, LeftReductive v) =>
    LeftReductive (MonoidMap k v)
  where
    isPrefixOf = isPrefixOf
    stripPrefix = stripPrefix

instance (Ord k, MonoidNull v, RightReductive v) =>
    RightReductive (MonoidMap k v)
  where
    isSuffixOf = isSuffixOf
    stripSuffix = stripSuffix

instance (Ord k, MonoidNull v, Reductive v) =>
    Reductive (MonoidMap k v)
  where
    (</>) = minusMaybe

instance (Ord k, MonoidNull v, LeftCancellative v) =>
    LeftCancellative (MonoidMap k v)

instance (Ord k, MonoidNull v, RightCancellative v) =>
    RightCancellative (MonoidMap k v)

instance (Ord k, MonoidNull v, Cancellative v) =>
    Cancellative (MonoidMap k v)

--------------------------------------------------------------------------------
-- Instances: Monoid and subclasses
--------------------------------------------------------------------------------

instance (Ord k, MonoidNull v) =>
    Monoid (MonoidMap k v)
  where
    mempty = empty

instance (Ord k, MonoidNull v) =>
    MonoidNull (MonoidMap k v)
  where
    null = null

instance (Ord k, PositiveMonoid v) =>
    PositiveMonoid (MonoidMap k v)

instance (Ord k, MonoidNull v, LeftGCDMonoid v) =>
    LeftGCDMonoid (MonoidMap k v)
  where
    commonPrefix = commonPrefix

instance (Ord k, MonoidNull v, RightGCDMonoid v) =>
    RightGCDMonoid (MonoidMap k v)
  where
    commonSuffix = commonSuffix

instance (Ord k, MonoidNull v, OverlappingGCDMonoid v) =>
    OverlappingGCDMonoid (MonoidMap k v)
  where
    overlap = overlap
    stripPrefixOverlap = stripPrefixOverlap
    stripSuffixOverlap = stripSuffixOverlap
    stripOverlap = stripOverlap

instance (Ord k, MonoidNull v, GCDMonoid v) =>
    GCDMonoid (MonoidMap k v)
  where
    gcd = gcd

instance (Ord k, MonoidNull v, Monus v) =>
    Monus (MonoidMap k v)
  where
    (<\>) = monus

--------------------------------------------------------------------------------
-- Instances: Group and subclasses
--------------------------------------------------------------------------------

instance (Ord k, MonoidNull v, Group v) =>
    Group (MonoidMap k v)
  where
    invert = invert
    (~~) = minus
    pow = power

instance (Ord k, MonoidNull v, Abelian v) =>
    Abelian (MonoidMap k v)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | \(O(1)\). The empty 'MonoidMap'.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k 'empty' == 'mempty'
-- @
--
-- Provides the definition of 'mempty' for the 'MonoidMap' instance of
-- 'Monoid'.
--
empty :: MonoidMap k v
empty = MonoidMap Map.empty

-- | \(O(n \log n)\). Constructs a 'MonoidMap' from a list of key-value pairs.
--
-- If the list contains more than one value for the same key, values are
-- combined together in the order that they appear with the '(<>)' operator.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('fromList' kvs) '=='
--     'foldMap' 'snd' ('L.filter' (('==' k) . fst) kvs)
-- @
--
-- Satisfies the following round-trip property:
--
-- @
-- 'fromList' ('toList' m) '==' m
-- @
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> 'fromList' [(1,"a"), (2,"x"), (1,"b"), (2,"y"), (1,"c"), (2,"z")]
-- 'fromList' [(1,"abc"), (2,"xyz")]
-- @
--
fromList :: (Ord k, MonoidNull v) => [(k, v)] -> MonoidMap k v
fromList = fromListWith (<>)

-- | \(O(n \log n)\). Constructs a 'MonoidMap' from a list of key-value pairs,
--   with a combining function for values.
--
-- If the list contains more than one value for the same key, values are
-- combined together in the order that they appear with the given combining
-- function.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('fromListWith' f kvs) '=='
--     'maybe' 'mempty' ('F.foldl1' f)
--         ('NE.nonEmpty' ('snd' '<$>' 'L.filter' (('==' k) . fst) kvs))
-- @
--
fromListWith
    :: (Ord k, MonoidNull v)
    => (v -> v -> v)
    -- ^ Function with which to combine values for duplicate keys.
    -> [(k, v)]
    -> MonoidMap k v
fromListWith f =
    -- The 'Map.fromListWith' function combines values for duplicate keys in
    -- /reverse order/, so we must flip the provided combining function.
    fromMap . Map.fromListWith (flip f)

-- | \(O(n)\). Constructs a 'MonoidMap' from an ordinary 'Map'.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('fromMap' m) '==' 'Map.findWithDefault' 'mempty' 'k' m
-- @
--
-- This function performs canonicalisation of 'C.null' values, and has a time
-- complexity that is linear in the length of the list.
--
-- For a version of this function that runs in constant time and does not
-- perform canonicalisation, see 'Data.Total.MonoidMap.Unsafe.unsafeFromMap'.
--
fromMap :: MonoidNull v => Map k v -> MonoidMap k v
fromMap = MonoidMap . Map.filter (not . C.null)

-- | \(O(1)\). Constructs a 'MonoidMap' from a single key-value pair.
--
-- Satisfies the following property:
--
-- @
-- 'get' 'k' ('singleton' k v) '==' v
-- @
--
-- Nullifying the value for key __@k@__ produces an 'empty' map:
--
-- @
-- 'nullify' 'k' ('singleton' k v) '==' 'empty'
-- @
--
singleton :: (Ord k, MonoidNull v) => k -> v -> MonoidMap k v
singleton k v = set k v mempty

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

-- | Converts a 'MonoidMap' to a list of key-value pairs, where the keys are in
--   ascending order.
--
-- The result only includes entries with values that are not 'C.null'.
--
-- Satisfies the following round-trip property:
--
-- @
-- 'fromList' ('toList' m) '==' m
-- @
--
-- The resulting list is sorted in ascending key order:
--
-- @
-- 'L.sortOn' 'fst' ('toList' m) '==' 'toList' m
-- @
--
toList :: MonoidMap k v -> [(k, v)]
toList = Map.toAscList . unMonoidMap

-- | Converts a 'MonoidMap' to an ordinary 'Map'.
--
-- The result only includes entries with values that are not 'C.null'.
--
-- Satisfies the following round-trip property:
--
-- @
-- 'fromMap' ('toMap' m) == m
-- @
--
toMap :: MonoidMap k v -> Map k v
toMap = unMonoidMap

--------------------------------------------------------------------------------
-- Basic operations
--------------------------------------------------------------------------------

-- | Gets the value associated with the given key.
--
-- By default, every key in an 'empty' map is associated with a value of
-- 'mempty':
--
-- @
-- ∀ k. 'get' k 'empty' '==' 'mempty'
-- @
--
get :: (Ord k, Monoid v) => k -> MonoidMap k v -> v
get k m = fromMaybe mempty $ Map.lookup k $ toMap m

-- | Sets the value associated with the given key.
--
-- Satisfies the following property:
--
-- @
-- 'get' k ('set' k v m) '==' v
-- @
--
set :: (Ord k, MonoidNull v) => k -> v -> MonoidMap k v -> MonoidMap k v
set k v m
    | C.null v  = MonoidMap $ Map.delete k   $ unMonoidMap m
    | otherwise = MonoidMap $ Map.insert k v $ unMonoidMap m

-- | Adjusts the value associated with the given key.
--
-- Satisfies the following property:
--
-- @
-- 'adjust' f k m '==' 'set' k (f ('get' k m)) m
-- @
--
adjust
    :: (Ord k, MonoidNull v)
    => (v -> v)
    -> k
    -> MonoidMap k v
    -> MonoidMap k v
adjust f k m = set k (f (get k m)) m

-- | Sets the value associated with the given key to 'mempty'.
--
-- Satisfies the following property:
--
-- @
-- 'get' k ('nullify' k m) '==' 'mempty'
-- @
--
nullify :: Ord k => k -> MonoidMap k v -> MonoidMap k v
nullify k (MonoidMap m) = MonoidMap $ Map.delete k m

--------------------------------------------------------------------------------
-- Membership
--------------------------------------------------------------------------------

-- | Returns 'True' if (and only if) all values in the map are 'C.null'.
--
-- Satisfies the following property:
--
-- @
-- 'null' m '==' (∀ k. 'nullKey' k m)
-- @
--
-- Provides the definition of 'C.null' for the 'MonoidMap' instance of
-- 'MonoidNull'.
--
null :: MonoidMap k v -> Bool
null = Map.null . toMap

-- | Returns 'True' if (and only if) the given key is associated with a value
--   that is 'C.null'.
--
-- Satisfies the following property:
--
-- @
-- 'nullKey' k m '==' 'C.null' ('get' k m)
-- @
--
nullKey :: Ord k => k -> MonoidMap k v -> Bool
nullKey k = Map.notMember k . toMap

-- | Returns 'True' if (and only if) the map contains at least one value that
--   is not 'C.null'.
--
-- Satisfies the following property:
--
-- @
-- 'nonNull' m '==' (∃ k. 'nonNullKey' k m)
-- @
--
nonNull :: MonoidMap k v -> Bool
nonNull = not . null

-- | Returns a count of all values in the map that are not 'C.null'.
--
-- Satisfies the following property:
--
-- @
-- 'nonNullCount' m '==' 'Set.size' ('nonNullKeys' m)
-- @
--
nonNullCount :: MonoidMap k v -> Int
nonNullCount = Map.size . toMap

-- | Returns 'True' if (and only if) the given key is associated with a value
--   that is not 'C.null'.
--
-- Satisfies the following property:
--
-- @
-- 'nonNullKey' k m '==' 'not' ('C.null' ('get' k m))
-- @
--
nonNullKey :: Ord k => k -> MonoidMap k v -> Bool
nonNullKey k = Map.member k . toMap

-- | Returns the set of keys associated with values that are not 'C.null'.
--
-- Satisfies the following property:
--
-- @
-- k '`Set.member`' ('nonNullKeys' m) '==' 'nonNullKey' k m
-- @
--
nonNullKeys :: MonoidMap k v -> Set k
nonNullKeys = Map.keysSet . toMap

--------------------------------------------------------------------------------
-- Slicing
--------------------------------------------------------------------------------

-- | /Takes/ a slice from a map.
--
-- This function takes a given number of non-'C.null' entries from a map,
-- producing a new map from the entries that were /taken/.
--
-- Entries are taken in /key order/, beginning with the /smallest/ keys.
--
-- Satifies the following property:
--
-- @
-- 'take' n '==' 'fromList' . 'Prelude.take' n . 'toList'
-- @
--
take :: Int -> MonoidMap k v -> MonoidMap k v
take i (MonoidMap m) = MonoidMap (Map.take i m)

-- | /Drops/ a slice from a map.
--
-- This function drops a given number of non-'C.null' entries from a map,
-- producing a new map from the entries that /remain/.
--
-- Entries are dropped in /key order/, beginning with the /smallest/ keys.
--
-- Satifies the following property:
--
-- @
-- 'drop' n '==' 'fromList' . 'Prelude.drop' n . 'toList'
-- @
--
drop :: Int -> MonoidMap k v -> MonoidMap k v
drop i (MonoidMap m) = MonoidMap (Map.drop i m)

-- | /Splits/ a map into /two/ slices.
--
-- This function is equivalent to a combination of 'take' and 'drop':
--
-- @
-- 'splitAt' n m '==' ('take' n m, 'drop' n m)
-- @
--
-- The resulting maps can be combined to reproduce the original map:
--
-- @
-- 'splitAt' n m '&'
--     \\(m1, m2) -> m1 '<>' m2 '==' m
-- @
--
-- The resulting maps have disjoint sets of non-'C.null' entries:
--
-- @
-- 'splitAt' n m '&'
--     \\(m1, m2) -> 'Set.disjoint' ('nonNullKeys' m1) ('nonNullKeys' m2)
-- @
--
splitAt :: Int -> MonoidMap k a -> (MonoidMap k a, MonoidMap k a)
splitAt i m = (take i m, drop i m)

--------------------------------------------------------------------------------
-- Filtering
--------------------------------------------------------------------------------

-- | Filters a map according to a predicate on /values/.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('filter' f m) '=='
--     if f ('get' k m)
--     then 'get' k m
--     else 'mempty'
-- @
--
-- The resulting map is identical to that obtained by constructing a map from a
-- filtered list of key-value pairs:
--
-- @
-- 'filter' f m '==' 'fromList' ('L.filter' (f . 'snd') ('toList' m))
-- @
--
filter :: (v -> Bool) -> MonoidMap k v -> MonoidMap k v
filter f (MonoidMap m) = MonoidMap $ Map.filter f m

-- | Filters a map according to a predicate on /keys/.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('filterKeys' f m) '=='
--     if f k
--     then 'get' k m
--     else 'mempty'
-- @
--
-- The resulting map is identical to that obtained by constructing a map from a
-- filtered list of key-value pairs:
--
-- @
-- 'filter' f m '==' 'fromList' ('L.filter' (f . 'fst') ('toList' m))
-- @
--
filterKeys :: (k -> Bool) -> MonoidMap k v -> MonoidMap k v
filterKeys f (MonoidMap m) = MonoidMap $ Map.filterWithKey (\k _ -> f k) m

-- | Filters a map according to a predicate on /keys and values/.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('filterWithKey' f m) '=='
--     if f k ('get' k m)
--     then 'get' k m
--     else 'mempty'
-- @
--
-- The resulting map is identical to that obtained by constructing a map from a
-- filtered list of key-value pairs:
--
-- @
-- 'filterWithKey' f m '==' 'fromList' ('L.filter' ('uncurry' f) ('toList' m))
-- @
--
filterWithKey :: (k -> v -> Bool) -> MonoidMap k v -> MonoidMap k v
filterWithKey f (MonoidMap m) = MonoidMap $ Map.filterWithKey f m

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

-- | Partitions a map according to a predicate on /values/.
--
-- Satisfies the following property:
--
-- @
-- 'partition' f m '=='
--     ( 'filter'  \   \   f  m
--     , 'filter' ('not' . f) m
--     )
-- @
--
-- The resulting maps can be combined to reproduce the original map:
--
-- @
-- 'partition' f m '&' \\(m1, m2) ->
--     m1 '<>' m2 '==' m
-- @
--
-- The resulting maps have disjoint sets of non-'C.null' entries:
--
-- @
-- 'partition' f m '&' \\(m1, m2) ->
--     'Set.disjoint'
--         ('nonNullKeys' m1)
--         ('nonNullKeys' m2)
-- @
--
partition :: (v -> Bool) -> MonoidMap k v -> (MonoidMap k v, MonoidMap k v)
partition f (MonoidMap m) =
    B.bimap MonoidMap MonoidMap $ Map.partition f m

-- | Partitions a map according to a predicate on /keys/.
--
-- Satisfies the following property:
--
-- @
-- 'partitionKeys' f m '=='
--     ( 'filterKeys'  \   \   f  m
--     , 'filterKeys' ('not' . f) m
--     )
-- @
--
-- The resulting maps can be combined to reproduce the original map:
--
-- @
-- 'partitionKeys' f m '&' \\(m1, m2) ->
--     m1 '<>' m2 '==' m
-- @
--
-- The resulting maps have disjoint sets of non-'C.null' entries:
--
-- @
-- 'partitionKeys' f m '&' \\(m1, m2) ->
--     'Set.disjoint'
--         ('nonNullKeys' m1)
--         ('nonNullKeys' m2)
-- @
--
partitionKeys
    :: (k -> Bool) -> MonoidMap k v -> (MonoidMap k v, MonoidMap k v)
partitionKeys f (MonoidMap m) =
    B.bimap MonoidMap MonoidMap $ Map.partitionWithKey (\k _ -> f k) m

-- | Partitions a map according to a predicate on /keys and values/.
--
-- Satisfies the following property:
--
-- @
-- 'partitionWithKey' f m '=='
--     ( 'filterWithKey'   \    \   \    \  \   \ f  m
--     , 'filterWithKey' (('fmap' . 'fmap') 'not' f) m
--     )
-- @
--
-- The resulting maps can be combined to reproduce the original map:
--
-- @
-- 'partitionWithKey' f m '&' \\(m1, m2) ->
--     m1 '<>' m2 '==' m
-- @
--
-- The resulting maps have disjoint sets of non-'C.null' entries:
--
-- @
-- 'partitionWithKey' f m '&' \\(m1, m2) ->
--     'Set.disjoint'
--         ('nonNullKeys' m1)
--         ('nonNullKeys' m2)
-- @
--
partitionWithKey
    :: (k -> v -> Bool) -> MonoidMap k v -> (MonoidMap k v, MonoidMap k v)
partitionWithKey f (MonoidMap m) =
    B.bimap MonoidMap MonoidMap $ Map.partitionWithKey f m

--------------------------------------------------------------------------------
-- Mapping
--------------------------------------------------------------------------------

-- | Applies a function to all non-'C.null' values of a 'MonoidMap'.
--
-- Satisfies the following properties for all functions __@f@__:
--
-- @
-- ('get' k m '==' 'mempty') ==> ('get' k ('map' f m) '==' 'mempty'     )
-- ('get' k m '/=' 'mempty') ==> ('get' k ('map' f m) '==' f ('get' k m))
-- @
--
-- If function __@f@__ preserves 'C.null' values, then the mapping is /total/
-- for all possible keys __@k@__:
--
-- @
-- (f 'mempty' '==' 'mempty') ==> (∀ k. 'get' k ('map' f m) '==' f ('get' k m)))
-- @
--
map
    :: MonoidNull v2
    => (v1 -> v2)
    -> MonoidMap k v1
    -> MonoidMap k v2
map f (MonoidMap m) = MonoidMap $ Map.mapMaybe (guardNotNull . f) m

-- | Applies a function to all the keys of a 'MonoidMap' that are associated
--   with non-'C.null' values.
--
-- If the resultant map would contain more than one value for the same key,
-- values are combined together in ascending key order with the '(<>)'
-- operator.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('mapKeys' f m) '=='
--     'F.foldMap'
--         ('`get`' m)
--         ('Set.filter' (('==') k . f) ('nonNullKeys' m))
-- @
--
mapKeys
    :: (Ord k2, MonoidNull v)
    => (k1 -> k2)
    -> MonoidMap k1 v
    -> MonoidMap k2 v
mapKeys = mapKeysWith (<>)

-- | Applies a function to all the keys of a 'MonoidMap' that are associated
--   with non-'C.null' values, with a combining function for values.
--
-- If the resultant map would contain more than one value for the same key,
-- values are combined together in ascending key order with the given
-- combining function.
--
-- Satisfies the following property:
--
-- @
-- 'mapKeysWith' c f '==' 'fromListWith' c . 'fmap' ('B.first' f) . 'toList'
-- @
--
mapKeysWith
    :: (Ord k2, MonoidNull v)
    => (v -> v -> v)
    -- ^ Function with which to combine values for duplicate keys.
    -> (k1 -> k2)
    -> MonoidMap k1 v
    -> MonoidMap k2 v
mapKeysWith combine fk (MonoidMap m)
    -- The 'Map.mapKeysWith' function combines values for duplicate keys in
    -- /descending order/, so we must flip the provided combining function.
    = MonoidMap
    $ Map.filter (not . C.null)
    $ Map.mapKeysWith (flip combine) fk m

--------------------------------------------------------------------------------
-- Association
--------------------------------------------------------------------------------

-- | Appends a pair of maps together.
--
-- Uses the 'Semigroup' operator '(<>)' to append each value in the first map
-- to its matching value in the second map.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('append' m1 m2) '==' 'get' k m1 '<>' 'get' k m2
-- @
--
-- This function provides the definition of '(<>)' for the 'MonoidMap' instance
-- of 'Semigroup'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> m1 = 'fromList' [(1, "abc"), (2, "ij" ), (3, "p"  )            ]
-- >>> m2 = 'fromList' [            (2, "  k"), (3,  "qr"), (4, "xyz")]
-- >>> m3 = 'fromList' [(1, "abc"), (2, "ijk"), (3, "pqr"), (4, "xyz")]
-- @
-- @
-- >>> 'append' m1 m2 '==' m3
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural.Natural' values:
--
-- @
-- >>> m1 = 'fromList' [("a", 4), ("b", 2), ("c", 1)          ]
-- >>> m2 = 'fromList' [          ("b", 1), ("c", 2), ("d", 4)]
-- >>> m3 = 'fromList' [("a", 4), ("b", 3), ("c", 3), ("d", 4)]
-- @
-- @
-- >>> 'append' m1 m2 '==' m3
-- 'True'
-- @
--
append
    :: (Ord k, MonoidNull v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
append = merge MergeStrategy
    { mergeNullWithNonNull =
        keepNonNull
        -- Justification:
        -- mempty <> v ≡ v

    , mergeNonNullWithNull =
        keepNonNull
        -- Justification:
        -- v <> mempty ≡ v

    , mergeNonNullWithNonNull =
        withBoth (<>)
    }

--------------------------------------------------------------------------------
-- Prefixes and suffixes
--------------------------------------------------------------------------------

-- | Indicates whether or not the first map is a /prefix/ of the second.
--
-- 'MonoidMap' __@m1@__ is a /prefix/ of 'MonoidMap' __@m2@__ if (and only if)
-- for all possible keys __@k@__, the value for __@k@__ in __@m1@__ is a
-- /prefix/ of the value for __@k@__ in __@m2@__:
--
-- @
-- m1 '`isPrefixOf`' m2 '==' (∀ k. 'get' k m1 '`C.isPrefixOf`' 'get' k m2)
-- @
--
-- This function provides the definition of 'C.isPrefixOf' for the 'MonoidMap'
-- instance of 'LeftReductive'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> m1 = 'fromList' [(1, "a"  ), (2, "p"  ), (3, "x"  )]
-- >>> m2 = 'fromList' [(1, "abc"), (2, "pqr"), (3, "xyz")]
-- >>> m1 '`isPrefixOf`' m2
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [            (2, "p"  )            ]
-- >>> m2 = 'fromList' [(1, "abc"), (2, "pqr"), (3, "xyz")]
-- >>> m1 '`isPrefixOf`' m2
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [(1, "abc"), (2, "p"  ), (3, "x"  )]
-- >>> m2 = 'fromList' [(1, "a"  ), (2, "pqr"), (3, "xyz")]
-- >>> m1 '`isPrefixOf`' m2
-- 'False'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural.Natural' values:
--
-- @
-- >>> m1 = 'fromList' [("a", 1), ("b", 1), ("c", 1)]
-- >>> m2 = 'fromList' [("a", 2), ("b", 4), ("c", 8)]
-- >>> m1 '`isPrefixOf`' m2
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [          ("b", 1)          ]
-- >>> m2 = 'fromList' [("a", 2), ("b", 4), ("c", 8)]
-- >>> m1 '`isPrefixOf`' m2
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", 2), ("b", 1), ("c", 1)]
-- >>> m2 = 'fromList' [("a", 1), ("b", 4), ("c", 8)]
-- >>> m1 '`isPrefixOf`' m2
-- 'False'
-- @
--
isPrefixOf
    :: (Ord k, Monoid v, LeftReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Bool
isPrefixOf m1 m2 =
    -- Note that in practice, it's sufficient to check the following property:
    --
    -- @
    -- m1 '`isPrefixOf`' m2 '=='
    --     'all'
    --         (\\k -> 'get' k m1 '`C.isPrefixOf`' 'get' k m2)
    --         ('nonNullKeys' m1)
    -- @
    --
    -- ==== Justification
    --
    -- According to the laws for 'LeftReductive':
    --
    -- @
    -- ∀ a b. b '`C.isPrefixOf`' (b '<>' a)
    -- @
    --
    -- Substituting 'mempty' for @b@:
    --
    -- @
    -- ∀ a. 'mempty' '`C.isPrefixOf`' ('mempty' '<>' a)
    -- @
    --
    -- According to the left identity law for 'Monoid':
    --
    -- @
    -- ∀ a. 'mempty' '<>' a '==' a
    -- @
    --
    -- We can therefore assert that:
    --
    -- @
    -- ∀ a. 'mempty' '`C.isPrefixOf`' a
    -- @
    --
    -- Since 'mempty' is /always/ a valid prefix, we only need to consider
    -- values in 'm1' that are /not/ 'mempty'.
    --
    -- The 'nonNullKeys' function, when applied to 'm1', gives us /precisely/
    -- the set of keys that are not associated with 'mempty' in 'm1':
    --
    -- @
    -- (k '`Data.Set.member`' 'nonNullKeys' m1) '==' ('get' k m1 '/=' 'mempty')
    -- @
    --
    all
        (\k -> get k m1 `C.isPrefixOf` get k m2)
        (nonNullKeys m1)

-- | Indicates whether or not the first map is a /suffix/ of the second.
--
-- 'MonoidMap' __@m1@__ is a /suffix/ of 'MonoidMap' __@m2@__ if (and only if)
-- for all possible keys __@k@__, the value for __@k@__ in __@m1@__ is a
-- /suffix/ of the value for __@k@__ in __@m2@__:
--
-- @
-- m1 '`isSuffixOf`' m2 '==' (∀ k. 'get' k m1 '`C.isSuffixOf`' 'get' k m2)
-- @
--
-- This function provides the definition of 'C.isSuffixOf' for the 'MonoidMap'
-- instance of 'RightReductive'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> m1 = 'fromList' [(1,   "c"), (2,   "r"), (3,   "z")]
-- >>> m2 = 'fromList' [(1, "abc"), (2, "pqr"), (3, "xyz")]
-- >>> m1 '`isSuffixOf`' m2
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [            (2,   "r")            ]
-- >>> m2 = 'fromList' [(1, "abc"), (2, "pqr"), (3, "xyz")]
-- >>> m1 '`isSuffixOf`' m2
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [(1, "abc"), (2,   "r"), (3,   "z")]
-- >>> m2 = 'fromList' [(1,   "c"), (2, "pqr"), (3, "xyz")]
-- >>> m1 '`isSuffixOf`' m2
-- 'False'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural.Natural' values:
--
-- @
-- >>> m1 = 'fromList' [("a", 1), ("b", 1), ("c", 1)]
-- >>> m2 = 'fromList' [("a", 2), ("b", 4), ("c", 8)]
-- >>> m1 '`isSuffixOf`' m2
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [          ("b", 1)          ]
-- >>> m2 = 'fromList' [("a", 2), ("b", 4), ("c", 8)]
-- >>> m1 '`isSuffixOf`' m2
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", 2), ("b", 1), ("c", 1)]
-- >>> m2 = 'fromList' [("a", 1), ("b", 4), ("c", 8)]
-- >>> m1 '`isSuffixOf`' m2
-- 'False'
-- @
--
isSuffixOf
    :: (Ord k, Monoid v, RightReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Bool
isSuffixOf m1 m2 =
    -- Note that in practice, it's sufficient to check the following property:
    --
    -- @
    -- m1 '`isSuffixOf`' m2 '=='
    --     'all'
    --         (\\k -> 'get' k m1 '`C.isSuffixOf`' 'get' k m2)
    --         ('nonNullKeys' m1)
    -- @
    --
    -- ==== Justification
    --
    -- According to the laws for 'RightReductive':
    --
    -- @
    -- ∀ a b. b '`C.isSuffixOf`' (a '<>' b)
    -- @
    --
    -- Substituting 'mempty' for @b@:
    --
    -- @
    -- ∀ a. 'mempty' '`C.isSuffixOf`' (a '<>' 'mempty')
    -- @
    --
    -- According to the right identity law for 'Monoid':
    --
    -- @
    -- ∀ a. a '<>' 'mempty' '==' a
    -- @
    --
    -- We can therefore assert that:
    --
    -- @
    -- ∀ a. 'mempty' '`C.isSuffixOf`' a
    -- @
    --
    -- Since 'mempty' is /always/ a valid suffix, we only need to consider
    -- values in 'm1' that are /not/ 'mempty'.
    --
    -- The 'nonNullKeys' function, when applied to 'm1', gives us /precisely/
    -- the set of keys that are not associated with 'mempty' in 'm1':
    --
    -- @
    -- (k '`Data.Set.member`' 'nonNullKeys' m1) '==' ('get' k m1 '/=' 'mempty')
    -- @
    --
    all
        (\k -> get k m1 `C.isSuffixOf` get k m2)
        (nonNullKeys m1)

-- | Strips a /prefix/ from a 'MonoidMap'.
--
-- If map __@m1@__ is a /prefix/ of map __@m2@__, then 'stripPrefix' __@m1@__
-- __@m2@__ will produce a /reduced/ map where prefix __@m1@__ is /stripped/
-- from __@m2@__.
--
-- === Properties
--
-- The 'stripPrefix' function, when applied to maps __@m1@__ and __@m2@__,
-- produces a result if (and only if) __@m1@__ is a prefix of __@m2@__:
--
-- @
-- 'isJust' ('stripPrefix' m1 m2) '==' m1 '`isPrefixOf`' m2
-- @
--
-- The value for any key __@k@__ in the result is /identical/ to the result of
-- stripping the value for __@k@__ in map __@m1@__ from the value for __@k@__
-- in map __@m2@__:
--
-- @
-- 'all'
--    (\\r -> 'Just' ('get' k r) '==' 'C.stripPrefix' ('get' k m1) ('get' k m2))
--    ('stripPrefix' m1 m2)
-- @
--
-- If we append prefix __@m1@__ to the /left-hand/ side of the result, we can
-- always recover the original map __@m2@__:
--
-- @
-- 'all'
--    (\\r -> m1 '<>' r '==' m2)
--    ('stripPrefix' m1 m2)
-- @
--
-- This function provides the definition of 'C.stripPrefix' for the 'MonoidMap'
-- instance of 'LeftReductive'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> __m1__ = 'fromList' [(1, ""   ), (2, "i"  ), (3, "pq" ), (4, "xyz")]
-- >>> __m2__ = 'fromList' [(1, "abc"), (2, "ijk"), (3, "pqr"), (4, "xyz")]
-- >>> __m3__ = 'fromList' [(1, "abc"), (2,  "jk"), (3,   "r"), (4,    "")]
-- @
-- @
-- >>> 'stripPrefix' __m1__ __m2__ '==' 'Just' __m3__
-- 'True'
-- @
-- @
-- >>> 'stripPrefix' __m2__ __m1__ '==' 'Nothing'
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural' values:
--
-- @
-- >>> __m1__ = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> __m2__ = 'fromList' [("a", 3), ("b", 3), ("c", 3), ("d", 3)]
-- >>> __m3__ = 'fromList' [("a", 3), ("b", 2), ("c", 1), ("d", 0)]
-- @
-- @
-- >>> 'stripPrefix' __m1__ __m2__ '==' 'Just' __m3__
-- 'True'
-- @
-- @
-- >>> 'stripPrefix' __m2__ __m1__ '==' 'Nothing'
-- 'True'
-- @
--
stripPrefix
    :: (Ord k, MonoidNull v, LeftReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Maybe (MonoidMap k v)
stripPrefix = mergeA MergeStrategy
    { mergeNonNullWithNull =
        withNonNullA (\v -> C.stripPrefix v mempty)

    , mergeNullWithNonNull =
        keepNonNull
        -- Justification:
        -- stripPrefix mempty a ≡ a

    , mergeNonNullWithNonNull =
        withBothA C.stripPrefix
    }

-- | Strips a /suffix/ from a 'MonoidMap'.
--
-- If map __@m1@__ is a /suffix/ of map __@m2@__, then 'stripSuffix' __@m1@__
-- __@m2@__ will produce a /reduced/ map where suffix __@m1@__ is /stripped/
-- from __@m2@__.
--
-- === Properties
--
-- The 'stripSuffix' function, when applied to maps __@m1@__ and __@m2@__,
-- produces a result if (and only if) __@m1@__ is a suffix of __@m2@__:
--
-- @
-- 'isJust' ('stripSuffix' m1 m2) '==' m1 '`isSuffixOf`' m2
-- @
--
-- The value for any key __@k@__ in the result is /identical/ to the result of
-- stripping the value for __@k@__ in map __@m1@__ from the value for __@k@__
-- in map __@m2@__:
--
-- @
-- 'all'
--    (\\r -> 'Just' ('get' k r) '==' 'C.stripSuffix' ('get' k m1) ('get' k m2))
--    ('stripSuffix' m1 m2)
-- @
--
-- If we append suffix __@m1@__ to the /right-hand/ side of the result, we can
-- always recover the original map __@m2@__:
--
-- @
-- 'all'
--    (\\r -> r '<>' m1 '==' m2)
--    ('stripSuffix' m1 m2)
-- @
--
-- This function provides the definition of 'C.stripSuffix' for the 'MonoidMap'
-- instance of 'RightReductive'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> __m1__ = 'fromList' [(1,    ""), (2,   "k"), (3,  "qr"), (4, "xyz")]
-- >>> __m2__ = 'fromList' [(1, "abc"), (2, "ijk"), (3, "pqr"), (4, "xyz")]
-- >>> __m3__ = 'fromList' [(1, "abc"), (2, "ij" ), (3, "p"  ), (4, ""   )]
-- @
-- @
-- >>> 'stripSuffix' __m1__ __m2__ '==' 'Just' __m3__
-- 'True'
-- @
-- @
-- >>> 'stripSuffix' __m2__ __m1__ '==' 'Nothing'
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural' values:
--
-- @
-- >>> __m1__ = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> __m2__ = 'fromList' [("a", 3), ("b", 3), ("c", 3), ("d", 3)]
-- >>> __m3__ = 'fromList' [("a", 3), ("b", 2), ("c", 1), ("d", 0)]
-- @
-- @
-- >>> 'stripSuffix' __m1__ __m2__ '==' 'Just' __m3__
-- 'True'
-- @
-- @
-- >>> 'stripSuffix' __m2__ __m1__ '==' 'Nothing'
-- 'True'
-- @
--
stripSuffix
    :: (Ord k, MonoidNull v, RightReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Maybe (MonoidMap k v)
stripSuffix = mergeA MergeStrategy
    { mergeNonNullWithNull =
        withNonNullA (\v -> C.stripSuffix v mempty)

    , mergeNullWithNonNull =
        keepNonNull
        -- Justification:
        -- stripSuffix mempty a ≡ a

    , mergeNonNullWithNonNull =
        withBothA C.stripSuffix
    }

-- | Finds the /greatest common prefix/ of two maps.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('commonPrefix' m1 m2)
--     '==' 'C.commonPrefix' ('get' k m1) ('get' k m2)
-- @
--
-- This function provides the definition of 'C.commonPrefix' for the
-- 'MonoidMap' instance of 'LeftGCDMonoid'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> __m1__ = 'fromList' [(1, "+++"), (2, "b++"), (3, "cc+"), (4, "ddd")]
-- >>> __m2__ = 'fromList' [(1, "---"), (2, "b--"), (3, "cc-"), (4, "ddd")]
-- >>> __m3__ = 'fromList' [(1, ""   ), (2, "b"  ), (3, "cc" ), (4, "ddd")]
-- @
-- @
-- >>> 'commonPrefix' __m1__ __m2__ '==' __m3__
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural' values:
--
-- @
-- >>> __m1__ = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> __m2__ = 'fromList' [("a", 2), ("b", 2), ("c", 2), ("d", 2)]
-- >>> __m3__ = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 2)]
-- @
-- @
-- >>> 'commonPrefix' __m1__ __m2__ '==' __m3__
-- 'True'
-- @
--
commonPrefix
    :: (Ord k, MonoidNull v, LeftGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
commonPrefix = merge MergeStrategy
    { mergeNonNullWithNull =
        keepNull
        -- Justification:
        -- commonPrefix a mempty ≡ mempty

    , mergeNullWithNonNull =
        keepNull
        -- Justification:
        -- commonPrefix mempty a ≡ mempty

    , mergeNonNullWithNonNull =
        withBoth C.commonPrefix
    }

-- | Finds the /greatest common suffix/ of two maps.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('commonSuffix' m1 m2)
--     '==' 'C.commonSuffix' ('get' k m1) ('get' k m2)
-- @
--
-- This function provides the definition of 'C.commonSuffix' for the
-- 'MonoidMap' instance of 'RightGCDMonoid'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> __m1__ = 'fromList' [(1, "+++"), (2, "++b"), (3, "+cc"), (4, "ddd")]
-- >>> __m2__ = 'fromList' [(1, "---"), (2, "--b"), (3, "-cc"), (4, "ddd")]
-- >>> __m3__ = 'fromList' [(1,    ""), (2,   "b"), (3,  "cc"), (4, "ddd")]
-- @
-- @
-- >>> 'commonSuffix' __m1__ __m2__ '==' __m3__
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural' values:
--
-- @
-- >>> __m1__ = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> __m2__ = 'fromList' [("a", 2), ("b", 2), ("c", 2), ("d", 2)]
-- >>> __m3__ = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 2)]
-- @
-- @
-- >>> 'commonSuffix' __m1__ __m2__ '==' __m3__
-- 'True'
-- @
--
commonSuffix
    :: (Ord k, MonoidNull v, RightGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
commonSuffix = merge MergeStrategy
    { mergeNonNullWithNull =
        keepNull
        -- Justification:
        -- commonSuffix a mempty ≡ mempty

    , mergeNullWithNonNull =
        keepNull
        -- Justification:
        -- commonSuffix mempty a ≡ mempty

    , mergeNonNullWithNonNull =
        withBoth C.commonSuffix
    }

-- | Strips the /greatest common prefix/ from a pair of maps.
--
-- Given two maps __@m1@__ and __@m2@__, 'stripCommonPrefix' produces a
-- tuple __@(p, r1, r2)@__, where:
--
--  - __@p@__ is the /greatest common prefix/ of __@m1@__ and __@m2@__
--  - __@r1@__ is the /remainder/ of stripping prefix __@p@__ from __@m1@__
--  - __@r2@__ is the /remainder/ of stripping prefix __@p@__ from __@m2@__
--
-- The resulting prefix __@p@__ can be appended to the /left-hand/ side of
-- either remainder __@r1@__ or __@r2@__ to /reproduce/ either of the original
-- maps __@m1@__ or __@m2@__ respectively:
--
-- @
-- 'stripCommonPrefix' m1 m2
--    '&' \\(p, r1, _) -> p '<>' r1 '==' m1
-- 'stripCommonPrefix' m1 m2
--    '&' \\(p, _, r2) -> p '<>' r2 '==' m2
-- @
--
-- Prefix __@p@__ is /identical/ to the result of applying 'commonPrefix' to
-- __@m1@__ and __@m2@__:
--
-- @
-- 'stripCommonPrefix' m1 m2
--    '&' \\(p, _, _) -> p '==' 'commonPrefix' m1 m2
-- @
--
-- Remainders __@r1@__ and __@r2@__ are /identical/ to the results of applying
-- 'stripPrefix' to __@p@__ and __@m1@__ or to __@p@__ and __@m2@__
-- respectively:
--
-- @
-- 'stripCommonPrefix' m1 m2
--    '&' \\(p, r1, _) -> 'Just' r1 '==' 'stripPrefix' p m1
-- 'stripCommonPrefix' m1 m2
--    '&' \\(p, _, r2) -> 'Just' r2 '==' 'stripPrefix' p m2
-- @
--
-- This function provides the definition of 'C.stripCommonPrefix' for the
-- 'MonoidMap' instance of 'LeftGCDMonoid'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> m1 = 'fromList' [(1, "+++"), (2, "a++"), (3, "aa+"), (4, "aaa")]
-- >>> m2 = 'fromList' [(1, "---"), (2, "a--"), (3, "aa-"), (4, "aaa")]
-- @
-- @
-- >>> p  = 'fromList' [(1, ""   ), (2, "a"  ), (3, "aa" ), (4, "aaa")]
-- >>> r1 = 'fromList' [(1, "+++"), (2,  "++"), (3,   "+"), (4,    "")]
-- >>> r2 = 'fromList' [(1, "---"), (2,  "--"), (3,   "-"), (4,    "")]
-- @
-- @
-- >>> 'stripCommonPrefix' m1 m2 '==' (p, r1, r2)
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural.Natural' values:
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3), ("e", 4)]
-- >>> m2 = 'fromList' [("a", 4), ("b", 3), ("c", 2), ("d", 1), ("e", 0)]
-- @
-- @
-- >>> p  = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 1), ("e", 0)]
-- >>> r1 = 'fromList' [("a", 0), ("b", 0), ("c", 0), ("d", 2), ("e", 4)]
-- >>> r2 = 'fromList' [("a", 4), ("b", 2), ("c", 0), ("d", 0), ("e", 0)]
-- @
-- @
-- >>> 'stripCommonPrefix' m1 m2 '==' (p, r1, r2)
-- 'True'
-- @
--
stripCommonPrefix
    :: (Ord k, MonoidNull v, LeftGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> (MonoidMap k v, MonoidMap k v, MonoidMap k v)
stripCommonPrefix = C.stripCommonPrefix

-- | Strips the /greatest common suffix/ from a pair of maps.
--
-- Given two maps __@m1@__ and __@m2@__, 'stripCommonSuffix' produces a
-- tuple __@(r1, r2, s)@__, where:
--
--  - __@s@__ is the /greatest common suffix/ of __@m1@__ and __@m2@__
--  - __@r1@__ is the /remainder/ of stripping suffix __@s@__ from __@m1@__
--  - __@r2@__ is the /remainder/ of stripping suffix __@s@__ from __@m2@__
--
-- The resulting suffix __@s@__ can be appended to the /right-hand/ side of
-- either remainder __@r1@__ or __@r2@__ to /reproduce/ either of the original
-- maps __@m1@__ or __@m2@__ respectively:
--
-- @
-- 'stripCommonSuffix' m1 m2
--    '&' \\(r1, _, s) -> r1 '<>' s '==' m1
-- 'stripCommonSuffix' m1 m2
--    '&' \\(_, r2, s) -> r2 '<>' s '==' m2
-- @
--
-- Suffix __@s@__ is /identical/ to the result of applying 'commonSuffix' to
-- __@m1@__ and __@m2@__:
--
-- @
-- 'stripCommonSuffix' m1 m2
--    '&' \\(_, _, s) -> s '==' 'commonSuffix' m1 m2
-- @
--
-- Remainders __@r1@__ and __@r2@__ are /identical/ to the results of applying
-- 'stripSuffix' to __@s@__ and __@m1@__ or to __@s@__ and __@m2@__
-- respectively:
--
-- @
-- 'stripCommonSuffix' m1 m2
--    '&' \\(r1, _, s) -> 'Just' r1 '==' 'stripSuffix' s m1
-- 'stripCommonSuffix' m1 m2
--    '&' \\(_, r2, s) -> 'Just' r2 '==' 'stripSuffix' s m2
-- @
--
-- This function provides the definition of 'C.stripCommonSuffix' for the
-- 'MonoidMap' instance of 'RightGCDMonoid'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> m1 = 'fromList' [(1, "+++"), (2, "++a"), (3, "+aa"), (4, "aaa")]
-- >>> m2 = 'fromList' [(1, "---"), (2, "--a"), (3, "-aa"), (4, "aaa")]
-- @
-- @
-- >>> r1 = 'fromList' [(1, "+++"), (2, "++" ), (3, "+"  ), (4, ""   )]
-- >>> r2 = 'fromList' [(1, "---"), (2, "--" ), (3, "-"  ), (4, ""   )]
-- >>> s  = 'fromList' [(1,    ""), (2,   "a"), (3,  "aa"), (4, "aaa")]
-- @
-- @
-- >>> 'stripCommonSuffix' m1 m2 '==' (r1, r2, s)
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural.Natural' values:
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3), ("e", 4)]
-- >>> m2 = 'fromList' [("a", 4), ("b", 3), ("c", 2), ("d", 1), ("e", 0)]
-- @
-- @
-- >>> r1 = 'fromList' [("a", 0), ("b", 0), ("c", 0), ("d", 2), ("e", 4)]
-- >>> r2 = 'fromList' [("a", 4), ("b", 2), ("c", 0), ("d", 0), ("e", 0)]
-- >>> s  = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 1), ("e", 0)]
-- @
-- @
-- >>> 'stripCommonSuffix' m1 m2 '==' (r1, r2, s)
-- 'True'
-- @
--
stripCommonSuffix
    :: (Ord k, MonoidNull v, RightGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> (MonoidMap k v, MonoidMap k v, MonoidMap k v)
stripCommonSuffix = C.stripCommonSuffix

--------------------------------------------------------------------------------
-- Overlap
--------------------------------------------------------------------------------

-- | Finds the /greatest overlap/ of two maps.
--
-- The /greatest overlap/ __@o@__ of maps __@m1@__ and __@m2@__ is the /unique/
-- greatest map that is both a /suffix/ of __@m1@__ and a /prefix/ of __@m2@__:
--
-- @
-- m1 '==' r1 '<>' o \  \
-- m2 '=='    \  \ o '<>' r2
-- @
--
-- Where:
--
--  - __@r1@__ is the /remainder/ obtained by stripping /suffix overlap/
--    __@o@__ from __@m1@__.
--
--      (see 'stripSuffixOverlap')
--
--  - __@r2@__ is the /remainder/ obtained by stripping /prefix overlap/
--    __@o@__ from __@m2@__.
--
--      (see 'stripPrefixOverlap')
--
-- This function satisfies the following property:
--
-- @
-- 'get' k ('overlap' m1 m2) '==' 'C.overlap' ('get' k m1) ('get' k m2)
-- @
--
-- This function provides the definition of 'C.overlap' for the 'MonoidMap'
-- instance of 'OverlappingGCDMonoid'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> m1 = 'fromList' [(1,"abc"   ), (2,"abcd"  ), (3,"abcde "), (4,"abcdef")]
-- >>> m2 = 'fromList' [(1,   "def"), (2,  "cdef"), (3," bcdef"), (4,"abcdef")]
-- >>> m3 = 'fromList' [(1,   ""   ), (2,  "cd"  ), (3," bcde" ), (4,"abcdef")]
-- @
-- @
-- >>> 'overlap' m1 m2 '==' m3
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural' values:
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3), ("e", 4)]
-- >>> m2 = 'fromList' [("a", 4), ("b", 3), ("c", 2), ("d", 1), ("e", 0)]
-- >>> m3 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 1), ("e", 0)]
-- @
-- @
-- >>> 'overlap' m1 m2 '==' m3
-- 'True'
-- @
--
overlap
    :: (Ord k, MonoidNull v, OverlappingGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
overlap = merge MergeStrategy
    { mergeNonNullWithNull =
        keepNull
        -- Justification:
        -- overlap a mempty ≡ mempty

    , mergeNullWithNonNull =
        keepNull
        -- Justification:
        -- overlap mempty a ≡ mempty

    , mergeNonNullWithNonNull =
        withBoth C.overlap
    }

-- | /Strips/ from the second map its /greatest prefix overlap/ with suffixes
--   of the first map.
--
-- Evaluating 'stripPrefixOverlap' __@m1@__ __@m2@__ produces the /remainder/
-- __@r2@__:
--
-- @
-- m1 '==' r1 '<>' o \  \
-- m2 '=='    \  \ o '<>' r2
-- @
--
-- Where __@o@__ is the /greatest overlap/ of maps __@m1@__ and __@m2@__: the
-- /unique/ greatest map that is both a /suffix/ of __@m1@__ and a /prefix/ of
-- __@m2@__.
--
-- This function satisfies the following property:
--
-- @
-- 'get' k ('stripPrefixOverlap' m1 m2)
--     '==' 'C.stripPrefixOverlap' ('get' k m1) ('get' k m2)
-- @
--
-- This function provides the definition of 'C.stripPrefixOverlap' for the
-- 'MonoidMap' instance of 'OverlappingGCDMonoid'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> m1 = 'fromList' [(1,"abc"   ), (2,"abcd"  ), (3,"abcde" ), (4,"abcdef")]
-- >>> m2 = 'fromList' [(1,   "def"), (2,  "cdef"), (3, "bcdef"), (4,"abcdef")]
-- >>> m3 = 'fromList' [(1,   "def"), (2,    "ef"), (3,     "f"), (4,      "")]
-- @
-- @
-- >>> 'stripPrefixOverlap' m1 m2 '==' m3
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural' values:
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3), ("e", 4)]
-- >>> m2 = 'fromList' [("a", 4), ("b", 3), ("c", 2), ("d", 1), ("e", 0)]
-- >>> m3 = 'fromList' [("a", 4), ("b", 2), ("c", 0), ("d", 0), ("e", 0)]
-- @
-- @
-- >>> 'stripPrefixOverlap' m1 m2 '==' m3
-- 'True'
-- @
--
stripPrefixOverlap
    :: (Ord k, MonoidNull v, OverlappingGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
stripPrefixOverlap = merge MergeStrategy
    { mergeNonNullWithNull =
        keepNull
        -- Justification:
        --
        -- overlap a b      <> stripPrefixOverlap a b      ≡ b
        -- overlap a mempty <> stripPrefixOverlap a mempty ≡ mempty
        --           mempty <> stripPrefixOverlap a mempty ≡ mempty
        --                     stripPrefixOverlap a mempty ≡ mempty

    , mergeNullWithNonNull =
        keepNonNull
        -- Justification:
        --
        -- overlap a      b <> stripPrefixOverlap a      b ≡ b
        -- overlap mempty b <> stripPrefixOverlap mempty b ≡ b
        --         mempty   <> stripPrefixOverlap mempty b ≡ b
        --                     stripPrefixOverlap mempty b ≡ b

    , mergeNonNullWithNonNull =
        withBoth C.stripPrefixOverlap
    }

-- | /Strips/ from the second map its /greatest suffix overlap/ with prefixes
--   of the first map.
--
-- Evaluating 'stripSuffixOverlap' __@m2@__ __@m1@__ produces the /remainder/
-- __@r1@__:
--
-- @
-- m1 '==' r1 '<>' o \  \
-- m2 '=='    \  \ o '<>' r2
-- @
--
-- Where __@o@__ is the /greatest overlap/ of maps __@m1@__ and __@m2@__: the
-- /unique/ greatest map that is both a /suffix/ of __@m1@__ and a /prefix/ of
-- __@m2@__.
--
-- This function satisfies the following property:
--
-- @
-- 'get' k ('stripSuffixOverlap' m2 m1)
--     '==' 'C.stripSuffixOverlap' ('get' k m2) ('get' k m1)
-- @
--
-- This function provides the definition of 'C.stripSuffixOverlap' for the
-- 'MonoidMap' instance of 'OverlappingGCDMonoid'.
--
-- === __Examples__
--
-- With 'String' values:
--
-- @
-- >>> m1 = 'fromList' [(1,"abc"   ), (2,"abcd"  ), (3,"abcde" ), (4,"abcdef")]
-- >>> m2 = 'fromList' [(1,   "def"), (2,  "cdef"), (3, "bcdef"), (4,"abcdef")]
-- >>> m3 = 'fromList' [(1,"abc"   ), (2,"ab"    ), (3,"a"     ), (4,""      )]
-- @
-- @
-- >>> 'stripSuffixOverlap' m2 m1 '==' m3
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural' values:
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3), ("e", 4)]
-- >>> m2 = 'fromList' [("a", 4), ("b", 3), ("c", 2), ("d", 1), ("e", 0)]
-- >>> m3 = 'fromList' [("a", 0), ("b", 0), ("c", 0), ("d", 2), ("e", 4)]
-- @
-- @
-- >>> 'stripSuffixOverlap' m2 m1 '==' m3
-- 'True'
-- @
--
stripSuffixOverlap
    :: (Ord k, MonoidNull v, OverlappingGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
stripSuffixOverlap = merge MergeStrategy
    { mergeNonNullWithNull =
        keepNull
        -- Justification:
        --
        -- stripSuffixOverlap b a      <> overlap a      b ≡ a
        -- stripSuffixOverlap b mempty <> overlap mempty b ≡ mempty
        -- stripSuffixOverlap b mempty <>         mempty   ≡ mempty
        -- stripSuffixOverlap b mempty                     ≡ mempty

    , mergeNullWithNonNull =
        keepNonNull
        -- Justification:
        --
        -- stripSuffixOverlap b      a <> overlap a b      ≡ a
        -- stripSuffixOverlap mempty a <> overlap a mempty ≡ a
        -- stripSuffixOverlap mempty a <>           mempty ≡ a
        -- stripSuffixOverlap mempty a                     ≡ a

    , mergeNonNullWithNonNull =
        withBoth C.stripSuffixOverlap
    }

-- | Finds the /greatest overlap/ of two maps and /strips/ it from both maps.
--
-- Evaluating 'stripOverlap' __@m1@__ __@m2@__ produces the tuple
-- __@(r1, o, r2)@__, where:
--
-- @
-- m1 '==' r1 '<>' o \  \
-- m2 '=='    \  \ o '<>' r2
-- @
--
-- Where:
--
--  - __@o@__ is the /greatest overlap/ of maps __@m1@__ and __@m2@__: the
--    /unique/ greatest map that is both a /suffix/ of __@m1@__ and a /prefix/
--    of __@m2@__.
--
--      (see 'overlap')
--
--  - __@r1@__ is the /remainder/ obtained by stripping /suffix overlap/
--    __@o@__ from __@m1@__.
--
--      (see 'stripSuffixOverlap')
--
--  - __@r2@__ is the /remainder/ obtained by stripping /prefix overlap/
--    __@o@__ from __@m2@__.
--
--      (see 'stripPrefixOverlap')
--
-- This function satisfies the following property:
--
-- @
-- 'stripOverlap' m1 m2 '=='
--    ( 'stripSuffixOverlap' m2 m1
--    , 'overlap' m1 m2
--    , 'stripPrefixOverlap' m1 m2
--    )
-- @
--
-- This function provides the definition of 'C.stripOverlap' for the
-- 'MonoidMap' instance of 'OverlappingGCDMonoid'.
--
stripOverlap
    :: (Ord k, MonoidNull v, OverlappingGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> (MonoidMap k v, MonoidMap k v, MonoidMap k v)
stripOverlap m1 m2 =
    ( stripSuffixOverlap m2 m1
    , m1 `overlap` m2
    , stripPrefixOverlap m1 m2
    )

--------------------------------------------------------------------------------
-- GCD
--------------------------------------------------------------------------------

-- | Finds the /greatest common divisor/ of two maps.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('gcd' m1 m2) '==' 'C.gcd' ('get' k m1) ('get' k m2)
-- @
--
-- This function provides the definition of 'C.gcd' for the 'MonoidMap'
-- instance of 'GCDMonoid'.
--
-- === __Examples__
--
-- With 'Data.Monoid.Product' 'Numeric.Natural.Natural' values, this function
-- computes the /greatest common divisor/ of each pair of matching values:
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> m2 = 'fromList' [("a", 0), ("b", 0), ("c", 0), ("d", 0)]
-- >>> m3 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- @
-- @
-- >>> 'gcd' m1 m2 '==' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> m2 = 'fromList' [("a", 2), ("b", 2), ("c", 2), ("d", 2)]
-- >>> m3 = 'fromList' [("a", 2), ("b", 1), ("c", 2), ("d", 1)]
-- @
-- @
-- >>> 'gcd' m1 m2 '==' m3
-- 'True'
-- @
--
-- With 'Set' 'Numeric.Natural.Natural' values, this function computes the
-- /set/ /intersection/ of each pair of matching values:
--
-- @
-- f xs = 'fromList' ('Set.fromList' '<$>' xs)
-- @
--
-- @
-- >>> m1 = f [("a", [0,1,2]), ("b", [3,4,5]), ("c", [6,7,8])]
-- >>> m2 = f [("a", [0    ]), ("b", [  4  ]), ("c", [    8])]
-- >>> m3 = f [("a", [0    ]), ("b", [  4  ]), ("c", [    8])]
-- @
-- @
-- >>> 'gcd' m1 m2 '==' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = f [("a", [0,1  ]), ("b", [3,4  ]), ("c", [6,7  ])]
-- >>> m2 = f [("a", [  1,2]), ("b", [  4,5]), ("c", [  7,8])]
-- >>> m3 = f [("a", [  1  ]), ("b", [  4  ]), ("c", [  7  ])]
-- @
-- @
-- >>> 'gcd' m1 m2 '==' m3
-- 'True'
-- @
--
gcd
    :: (Ord k, MonoidNull v, GCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
gcd = merge MergeStrategy
    { mergeNonNullWithNull =
        keepNull
        -- Justification:
        -- gcd a b      ≡ commonPrefix a b      ≡ commonSuffix a b
        -- gcd a mempty ≡ commonPrefix a mempty ≡ commonSuffix a mempty
        -- gcd a mempty ≡                mempty ≡                mempty

    , mergeNullWithNonNull =
        keepNull
        -- Justification:
        -- gcd a      b ≡ commonPrefix a      b ≡ commonSuffix a      b
        -- gcd mempty b ≡ commonPrefix mempty b ≡ commonSuffix mempty b
        -- gcd mempty b ≡              mempty   ≡              mempty

    , mergeNonNullWithNonNull =
        withBoth C.gcd
    }

--------------------------------------------------------------------------------
-- Subtraction
--------------------------------------------------------------------------------

-- | Performs /group subtraction/ of the second map from the first.
--
-- Uses the 'Group' subtraction operator '(C.~~)' to subtract each value in the
-- second map from its matching value in the first map.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k (m1 '`minus`' m2) '==' 'get' k m1 'C.~~' 'get' k m2
-- @
--
-- This function provides the definition of '(C.~~)' for the 'MonoidMap'
-- instance of 'Group'.
--
-- === __Examples__
--
-- With 'Data.Monoid.Sum' 'Integer' values, this function performs normal
-- integer subtraction of matching values:
--
-- @
-- >>> m1 = 'fromList' [("a", (-1)), ("b",   0 ), ("c", 1)]
-- >>> m2 = 'fromList' [("a",   1 ), ("b",   1 ), ("c", 1)]
-- >>> m3 = 'fromList' [("a", (-2)), ("b", (-1)), ("c", 0)]
-- @
-- @
-- >>> m1 '`minus`' m2 '==' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", (-1)), ("b",   0 ), ("c",   1 )]
-- >>> m2 = 'fromList' [("a", (-1)), ("b", (-1)), ("c", (-1))]
-- >>> m3 = 'fromList' [("a",   0 ), ("b",   1 ), ("c",   2 )]
-- @
-- @
-- >>> m1 '`minus`' m2 '==' m3
-- 'True'
-- @
--
minus
    :: (Ord k, MonoidNull v, Group v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
minus = merge MergeStrategy
    { mergeNonNullWithNull =
        keepNonNull
        -- Justification:
        -- a ~~ mempty ≡ a

    , mergeNullWithNonNull =
        withNonNull C.invert
        -- Justification:
        -- a      ~~ b ≡ a      <> invert b
        -- mempty ~~ b ≡ mempty <> invert b
        -- mempty ~~ b ≡           invert b

    , mergeNonNullWithNonNull =
        withBoth (C.~~)
    }

-- | Performs /reductive subtraction/ of the second map from the first.
--
-- Uses the 'Reductive' subtraction operator '(</>)' to subtract each value in
-- the second map from its matching value in the first map.
--
-- This function produces a result if (and only if) for all possible keys
-- __@k@__, it is possible to subtract the value for __@k@__ in the second map
-- from the value for __@k@__ in the first map:
--
-- @
-- 'isJust' (m1 '`minusMaybe`' m2)
--     '==' (∀ k. 'isJust' ('get' k m1 '</>' 'get' k m2))
-- @
--
-- Otherwise, this function returns 'Nothing'.
--
-- This function satisfies the following property:
--
-- @
-- 'all'
--    (\\r -> 'Just' ('get' k r) '==' 'get' k m1 '</>' 'get' k m2)
--    (m1 '`minusMaybe`' m2)
-- @
--
-- This function provides the definition of '(</>)' for the 'MonoidMap'
-- instance of 'Reductive'.
--
-- === __Examples__
--
-- With 'Set' 'Numeric.Natural.Natural' values, this function performs /set/
-- /subtraction/ of matching values, succeeding if (and only if) each value
-- from the second map is a subset of its matching value from the first map:
--
-- @
-- f xs = 'fromList' ('Set.fromList' '<$>' xs)
-- @
--
-- @
-- >>> m1 = f [("a", [0,1,2]), ("b", [0,1,2])]
-- >>> m2 = f [("a", [     ]), ("b", [0,1,2])]
-- >>> m3 = f [("a", [0,1,2]), ("b", [     ])]
-- @
-- @
-- >>> m1 '`minusMaybe`' m2 '==' 'Just' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = f [("a", [0,1,2]), ("b", [0,1,2]), ("c", [0,1,2])]
-- >>> m2 = f [("a", [0    ]), ("b", [  1  ]), ("c", [    2])]
-- >>> m3 = f [("a", [  1,2]), ("b", [0,  2]), ("c", [0,1  ])]
-- @
-- @
-- >>> m1 '`minusMaybe`' m2 '==' 'Just' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = f [("a", [0,1,2    ]), ("b", [0,1,2    ]), ("c", [0,1,2    ])]
-- >>> m2 = f [("a", [    2,3,4]), ("b", [  1,2,3,4]), ("c", [0,1,2,3,4])]
-- @
-- @
-- >>> m1 '`minusMaybe`' m2 '==' 'Nothing'
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural.Natural' values, this function
-- performs /ordinary/ /subtraction/ of matching values, succeeding if (and only
-- if) each value from the second map is less than or equal to its matching
-- value from the first map:
--
-- @
-- >>> m1 = 'fromList' [("a", 2), ("b", 3), ("c", 5), ("d", 8)]
-- >>> m2 = 'fromList' [("a", 0), ("b", 0), ("c", 0), ("d", 0)]
-- >>> m3 = 'fromList' [("a", 2), ("b", 3), ("c", 5), ("d", 8)]
-- @
-- @
-- >>> m1 '`minusMaybe`' m2 '==' 'Just' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", 2), ("b", 3), ("c", 5), ("d", 8)]
-- >>> m2 = 'fromList' [("a", 1), ("b", 2), ("c", 3), ("d", 5)]
-- >>> m3 = 'fromList' [("a", 1), ("b", 1), ("c", 2), ("d", 3)]
-- @
-- @
-- >>> m1 '`minusMaybe`' m2 '==' 'Just' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", 2), ("b", 3), ("c", 5), ("d", 8)]
-- >>> m2 = 'fromList' [("a", 2), ("b", 3), ("c", 5), ("d", 8)]
-- >>> m3 = 'fromList' [("a", 0), ("b", 0), ("c", 0), ("d", 0)]
-- @
-- @
-- >>> m1 '`minusMaybe`' m2 '==' 'Just' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", 2), ("b", 3), ("c", 5), ("d", 8)]
-- >>> m2 = 'fromList' [("a", 3), ("b", 3), ("c", 5), ("d", 8)]
-- @
-- @
-- >>> m1 '`minusMaybe`' m2 '==' 'Nothing'
-- 'True'
-- @
--
minusMaybe
    :: (Ord k, MonoidNull v, Reductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Maybe (MonoidMap k v)
minusMaybe = mergeA MergeStrategy
    { mergeNonNullWithNull =
        keepNonNull
        -- Justification:
        --
        -- According to laws for Reductive:
        -- maybe a (b      <>) (a </> b     ) ≡       a
        -- maybe a (mempty <>) (a </> mempty) ≡       a
        -- maybe a (id       ) (a </> mempty) ≡       a
        --                     (a </> mempty) ∈ {Just a, Nothing}
        --
        -- According to laws for LeftReductive and RightReductive:
        -- isJust (a </> b     ) ≡ b      `isPrefixOf` a ≡ b      `isSuffixOf` a
        -- isJust (a </> mempty) ≡ mempty `isPrefixOf` a ≡ mempty `isSuffixOf` a
        --
        -- According to laws for LeftReductive and RightReductive:
        -- b      `isPrefixOf` (b      <> a)
        -- mempty `isPrefixOf` (mempty <> a)
        -- mempty `isPrefixOf`            a
        --
        -- Therefore:
        -- a </> mempty ≡ Just a
        --
    , mergeNullWithNonNull =
        withNonNullA (\v -> mempty </> v)

    , mergeNonNullWithNonNull =
        withBothA (</>)
    }

-- | Performs /monus subtraction/ of the second map from the first.
--
-- Uses the 'Monus' subtraction operator '(<\>)' to subtract each value in
-- the second map from its matching value in the first map.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k (m1 '`monus`' m2) '==' 'get' k m1 '<\>' 'get' k m2
-- @
--
-- This function provides the definition of '(<\>)' for the 'MonoidMap'
-- instance of 'Monus'.
--
-- === __Examples__
--
-- With 'Set' 'Numeric.Natural.Natural' values, this function performs /set/
-- /subtraction/ of matching values:
--
-- @
-- f xs = 'fromList' ('Set.fromList' '<$>' xs)
-- @
--
-- @
-- >>> m1 = f [("a", [0,1,2]), ("b", [0,1,2])]
-- >>> m2 = f [("a", [     ]), ("b", [0,1,2])]
-- >>> m3 = f [("a", [0,1,2]), ("b", [     ])]
-- @
-- @
-- >>> m1 '`monus`' m2 '==' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = f [("a", [0,1,2]), ("b", [0,1,2]), ("c", [0,1,2])]
-- >>> m2 = f [("a", [0    ]), ("b", [  1  ]), ("c", [    2])]
-- >>> m3 = f [("a", [  1,2]), ("b", [0,  2]), ("c", [0,1  ])]
-- @
-- @
-- >>> m1 '`monus`' m2 '==' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = f [("a", [0,1,2    ]), ("b", [0,1,2    ]), ("c", [0,1,2    ])]
-- >>> m2 = f [("a", [    2,3,4]), ("b", [  1,2,3,4]), ("c", [0,1,2,3,4])]
-- >>> m3 = f [("a", [0,1      ]), ("b", [0        ]), ("c", [         ])]
-- @
-- @
-- >>> m1 '`monus`' m2 '==' m3
-- 'True'
-- @
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural.Natural' values, this function
-- performs /truncated/ /subtraction/ of matching values:
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> m2 = 'fromList' [("a", 0), ("b", 0), ("c", 0), ("d", 0)]
-- >>> m3 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- @
-- @
-- >>> m1 '`monus`' m2 '==' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> m2 = 'fromList' [("a", 1), ("b", 1), ("c", 1), ("d", 1)]
-- >>> m3 = 'fromList' [("a", 0), ("b", 0), ("c", 1), ("d", 2)]
-- @
-- @
-- >>> m1 '`monus`' m2 '==' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> m2 = 'fromList' [("a", 2), ("b", 2), ("c", 2), ("d", 2)]
-- >>> m3 = 'fromList' [("a", 0), ("b", 0), ("c", 0), ("d", 1)]
-- @
-- @
-- >>> m1 '`monus`' m2 '==' m3
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> m2 = 'fromList' [("a", 4), ("b", 4), ("c", 4), ("d", 4)]
-- >>> m3 = 'fromList' [("a", 0), ("b", 0), ("c", 0), ("d", 0)]
-- @
-- @
-- >>> m1 '`monus`' m2 '==' m3
-- 'True'
-- @
--
monus
    :: (Ord k, MonoidNull v, Monus v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
monus = merge MergeStrategy
    { mergeNullWithNonNull =
        keepNull
        -- Justification:
        -- mempty <\> a ≡ mempty

    , mergeNonNullWithNull =
        keepNonNull
        -- Justification:
        -- a      <> (b <\> a     ) ≡ b <> (a      <\> b)
        -- mempty <> (b <\> mempty) ≡ b <> (mempty <\> a)
        --            b <\> mempty  ≡ b <> (mempty <\> a)
        --            b <\> mempty  ≡ b <>  mempty
        --            b <\> mempty  ≡ b

    , mergeNonNullWithNonNull =
        withBoth (<\>)
    }

--------------------------------------------------------------------------------
-- Inversion
--------------------------------------------------------------------------------

-- | Inverts every value in a map.
--
-- Applies the 'Group' method 'C.invert' to every value in a map.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k ('invert' m) '==' 'C.invert' ('get' k m)
-- @
--
-- This function provides the definition of 'C.invert' for the 'MonoidMap'
-- instance of 'Group'.
--
-- === __Examples__
--
-- With 'Data.Monoid.Sum' 'Integer' values, this function performs negation
-- of values:
--
-- @
-- >>> m1 = 'fromList' [("a", (-1)), ("b", 0), ("c",   1) ]
-- >>> m2 = 'fromList' [("a",   1 ), ("b", 0), ("c", (-1))]
-- @
-- @
-- >>> 'negate' m1 '==' m2
-- 'True'
-- @
--
invert
    :: (Ord k, MonoidNull v, Group v)
    => MonoidMap k v
    -> MonoidMap k v
invert = map C.invert

--------------------------------------------------------------------------------
-- Exponentiation
--------------------------------------------------------------------------------

-- | Performs exponentiation of every value in a map.
--
-- Uses the 'Group' exponentiation method 'C.pow' to raise every value in a map
-- to the power of the given exponent.
--
-- Satisfies the following property for all possible keys __@k@__:
--
-- @
-- 'get' k (m '`power`' i) '==' 'get' k m '`C.pow`' i
-- @
--
-- This function provides the definition of 'C.pow' for the 'MonoidMap'
-- instance of 'Group'.
--
-- === __Examples__
--
-- With 'Data.Monoid.Sum' 'Numeric.Natural.Natural' values, this function
-- performs /ordinary multiplication/ of all values by the given exponent:
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
-- >>> m2 = 'fromList' [("a", 0), ("b", 2), ("c", 4), ("d", 6)]
-- @
-- @
-- >>> m1 '`power`' 2 '==' m2
-- 'True'
-- @
--
-- @
-- >>> m1 = 'fromList' [("a", 0), ("b",   1 ), ("c",   2 ), ("d",   3 )]
-- >>> m2 = 'fromList' [("a", 0), ("b", (-1)), ("c", (-2)), ("d", (-3))]
-- @
-- @
-- >>> m1 '`power`' (-1) '==' m2
-- 'True'
-- @
--
power
    :: (Integral i, MonoidNull v, Group v)
    => MonoidMap k v
    -> i
    -> MonoidMap k v
power m i = map (`C.pow` i) m

--------------------------------------------------------------------------------
-- Intersection
--------------------------------------------------------------------------------

intersection
    :: (Ord k, MonoidNull v3)
    => (v1 -> v2 -> v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> MonoidMap k v3
intersection f = merge MergeStrategy
    { mergeNullWithNonNull =
        keepNull
    , mergeNonNullWithNull =
        keepNull
    , mergeNonNullWithNonNull =
        withBoth f
    }

intersectionA
    :: (Applicative f, Ord k, MonoidNull v3)
    => (v1 -> v2 -> f v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> f (MonoidMap k v3)
intersectionA f = mergeA MergeStrategy
    { mergeNullWithNonNull =
        keepNull
    , mergeNonNullWithNull =
        keepNull
    , mergeNonNullWithNonNull =
        withBothA f
    }

--------------------------------------------------------------------------------
-- Union
--------------------------------------------------------------------------------

union
    :: (Ord k, Monoid v1, Monoid v2, MonoidNull v3)
    => (v1 -> v2 -> v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> MonoidMap k v3
union f = merge MergeStrategy
    { mergeNullWithNonNull =
        withNonNull (\v -> f mempty v)
    , mergeNonNullWithNull =
        withNonNull (\v -> f v mempty)
    , mergeNonNullWithNonNull =
        withBoth f
    }

unionA
    :: (Applicative f, Ord k, Monoid v1, Monoid v2, MonoidNull v3)
    => (v1 -> v2 -> f v3)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> f (MonoidMap k v3)
unionA f = mergeA MergeStrategy
    { mergeNullWithNonNull =
        withNonNullA (\v -> f mempty v)
    , mergeNonNullWithNull =
        withNonNullA (\v -> f v mempty)
    , mergeNonNullWithNonNull =
        withBothA f
    }

--------------------------------------------------------------------------------
-- Merging
--------------------------------------------------------------------------------

type WhenOneSideNull f k v     v3 = Map.WhenMissing f k v     v3
type WhenBothNonNull f k v1 v2 v3 = Map.WhenMatched f k v1 v2 v3

data MergeStrategy f k v1 v2 v3 = MergeStrategy
    { mergeNonNullWithNull    :: WhenOneSideNull f k v1    v3
    , mergeNullWithNonNull    :: WhenOneSideNull f k    v2 v3
    , mergeNonNullWithNonNull :: WhenBothNonNull f k v1 v2 v3
    }

merge
    :: Ord k
    => MergeStrategy Identity k v1 v2 v3
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> MonoidMap k v3
merge strategy m1 m2 =
    MonoidMap $
        Map.merge
            (strategy & mergeNonNullWithNull)
            (strategy & mergeNullWithNonNull)
            (strategy & mergeNonNullWithNonNull)
            (unMonoidMap m1)
            (unMonoidMap m2)

mergeA
    :: (Applicative f, Ord k)
    => MergeStrategy f k v1 v2 v3
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> f (MonoidMap k v3)
mergeA strategy m1 m2 =
    MonoidMap <$>
        Map.mergeA
            (strategy & mergeNonNullWithNull)
            (strategy & mergeNullWithNonNull)
            (strategy & mergeNonNullWithNonNull)
            (unMonoidMap m1)
            (unMonoidMap m2)

keepNull
    :: Applicative f
    => WhenOneSideNull f k v1 v2
keepNull = Map.dropMissing

keepNonNull
    :: Applicative f
    => WhenOneSideNull f k v v
keepNonNull = Map.preserveMissing

withNonNull
    :: (Applicative f, MonoidNull v2)
    => (v1 -> v2)
    -> WhenOneSideNull f k v1 v2
withNonNull f = Map.mapMaybeMissing $ \_k v -> guardNotNull $ f v

withNonNullA
    :: (Applicative f, MonoidNull v2)
    => (v1 -> f v2)
    -> WhenOneSideNull f k v1 v2
withNonNullA f = Map.traverseMaybeMissing $ \_k v -> guardNotNull <$> f v

withBoth
    :: (Applicative f, MonoidNull v3)
    => (v1 -> v2 -> v3)
    -> WhenBothNonNull f k v1 v2 v3
withBoth f = Map.zipWithMaybeMatched $ \_k v1 v2 -> guardNotNull $ f v1 v2

withBothA
    :: (Applicative f, MonoidNull v3)
    => (v1 -> v2 -> f v3)
    -> WhenBothNonNull f k v1 v2 v3
withBothA f = Map.zipWithMaybeAMatched $ \_k v1 v2 -> guardNotNull <$> f v1 v2

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

guardNotNull :: MonoidNull v => v -> Maybe v
guardNotNull v
    | C.null v = Nothing
    | otherwise = Just v
{-# INLINE guardNotNull #-}
