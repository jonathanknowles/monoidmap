-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- A __lawful__ implementation of 'MultiMap', implemented in terms of 'Map' and
-- 'NESet'.
--
module Data.MonoidMap.Examples.MultiMap.Instances.MultiMap3 where

import Prelude

import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.MonoidMap.Examples.Set.NonEmpty
    ( NESet )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.MonoidMap.Examples.Set.NonEmpty as NESet
import qualified Data.MonoidMap.Examples.MultiMap.Class as Class

newtype MultiMap3 k v = MultiMap (Map k (NESet v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => Class.MultiMap MultiMap3 k v where

    fromList
        = MultiMap
        . Map.fromListWith (<>)
        . mapMaybe (traverse NESet.nonEmptySet)

    toList (MultiMap m) = fmap NESet.toSet <$> Map.toList m

    empty = MultiMap Map.empty

    lookup k (MultiMap m) = maybe Set.empty NESet.toSet (Map.lookup k m)

    null (MultiMap m) = Map.null m

    nonNull (MultiMap m) = not (Map.null m)

    nonNullKey k (MultiMap m) = Map.member k m

    nonNullKeys (MultiMap m) = Map.keysSet m

    nonNullCount (MultiMap m) = Map.size m

    isSubmapOf (MultiMap m1) (MultiMap m2) =
        Map.isSubmapOfBy NESet.isSubsetOf m1 m2

    update k vs (MultiMap m) =
        case NESet.nonEmptySet vs of
            Nothing -> MultiMap (Map.delete k    m)
            Just ys -> MultiMap (Map.insert k ys m)

    insert k vs (MultiMap m) =
        case NESet.nonEmptySet xs of
            Nothing -> MultiMap (Map.delete k    m)
            Just ys -> MultiMap (Map.insert k ys m)
      where
        xs = maybe Set.empty NESet.toSet (Map.lookup k m) `Set.union` vs

    remove k vs (MultiMap m) =
        case NESet.nonEmptySet xs of
            Nothing -> MultiMap (Map.delete k    m)
            Just ys -> MultiMap (Map.insert k ys m)
      where
        xs = maybe Set.empty NESet.toSet (Map.lookup k m) `Set.difference` vs

    union (MultiMap m1) (MultiMap m2) = MultiMap $
        Map.unionWith NESet.union m1 m2

    intersection (MultiMap m1) (MultiMap m2) = MultiMap $
        Map.merge
            Map.dropMissing
            Map.dropMissing
            (Map.zipWithMaybeMatched mergeValues)
            m1
            m2
      where
        mergeValues :: Ord v => k -> NESet v -> NESet v -> Maybe (NESet v)
        mergeValues _k s1 s2 = NESet.nonEmptySet (NESet.intersection s1 s2)
