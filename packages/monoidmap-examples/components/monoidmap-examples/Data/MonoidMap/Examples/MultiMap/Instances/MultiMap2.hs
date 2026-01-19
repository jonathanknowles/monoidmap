-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- A __lawful__ implementation of 'MultiMap', implemented in terms of 'Map' and
-- 'Set'.
--
module Data.MonoidMap.Examples.MultiMap.Instances.MultiMap2 where

import Prelude

import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.MonoidMap.Examples.MultiMap.Class as Class

newtype MultiMap2 k v = MultiMap (Map k (Set v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => Class.MultiMap MultiMap2 k v where

    fromList = MultiMap . Map.fromListWith (<>) . filter ((/= mempty) . snd)

    toList (MultiMap m) = Map.toList m

    empty = MultiMap Map.empty

    lookup k (MultiMap m) = Map.findWithDefault Set.empty k m

    null (MultiMap m) = Map.null m

    nonNull (MultiMap m) = not (Map.null m)

    nonNullKey k (MultiMap m) = Map.member k m

    nonNullKeys (MultiMap m) = Map.keysSet m

    nonNullCount (MultiMap m) = Map.size m

    isSubmapOf (MultiMap m1) (MultiMap m2) =
        Map.isSubmapOfBy Set.isSubsetOf m1 m2

    update k vs (MultiMap m)
        | Set.null vs = MultiMap (Map.delete k    m)
        | otherwise   = MultiMap (Map.insert k vs m)

    insert k vs (MultiMap m)
        | Set.null xs = MultiMap (Map.delete k    m)
        | otherwise   = MultiMap (Map.insert k xs m)
      where
        xs = Map.findWithDefault Set.empty k m `Set.union` vs

    remove k vs (MultiMap m)
        | Set.null xs = MultiMap (Map.delete k    m)
        | otherwise   = MultiMap (Map.insert k xs m)
      where
        xs = Map.findWithDefault Set.empty k m `Set.difference` vs

    union (MultiMap m1) (MultiMap m2) = MultiMap $
        Map.unionWith Set.union m1 m2

    intersection (MultiMap m1) (MultiMap m2) = MultiMap $
        Map.merge
            Map.dropMissing
            Map.dropMissing
            (Map.zipWithMaybeMatched mergeValues)
            m1
            m2
      where
        mergeValues :: k -> Set v -> Set v -> Maybe (Set v)
        mergeValues _k s1 s2
            | Set.null s3 = Nothing
            | otherwise   = Just s3
          where
            s3 = Set.intersection s1 s2
