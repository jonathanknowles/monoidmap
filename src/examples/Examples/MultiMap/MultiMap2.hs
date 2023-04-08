-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- A lawful implementation of 'MultiMap', implemented in terms of 'Map' and
-- 'Set'.
--
module Examples.MultiMap.MultiMap2 where

import Prelude hiding
    ( lookup )

import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Examples.MultiMap
    ( MultiMap (..) )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype MultiMap2 k v = MultiMap (Map k (Set v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => MultiMap MultiMap2 k v where

    empty = MultiMap Map.empty

    fromList = MultiMap . Map.fromListWith (<>) . filter ((/= mempty) . snd)

    toList (MultiMap m) = Map.toList m

    null (MultiMap m) = Map.null m

    nonNullKey k (MultiMap m) = Map.member k m

    nonNullKeys (MultiMap m) = Map.keysSet m

    nonNullCount (MultiMap m) = Map.size m

    lookup k (MultiMap m) = Map.findWithDefault Set.empty k m

    update k vs (MultiMap m)
        | Set.null vs = MultiMap (Map.delete k    m)
        | otherwise   = MultiMap (Map.insert k vs m)

    insert k vs (MultiMap m)
        | Set.null zs = MultiMap (Map.delete k    m)
        | otherwise   = MultiMap (Map.insert k zs m)
      where
        zs = lookup k (MultiMap m) `Set.union` vs

    remove k vs (MultiMap m)
        | Set.null zs = MultiMap (Map.delete k    m)
        | otherwise   = MultiMap (Map.insert k zs m)
      where
        zs = lookup k (MultiMap m) `Set.difference` vs

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

    isSubMultiMapOf (MultiMap m1) (MultiMap m2) =
        Map.isSubmapOfBy Set.isSubsetOf m1 m2
