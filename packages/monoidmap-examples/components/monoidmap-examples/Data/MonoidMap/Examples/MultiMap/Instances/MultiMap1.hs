-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- An __unlawful__ implementation of 'MultiMap', implemented in terms of 'Map'
-- and 'Set'.
--
-- This implementation has several subtle bugs. 💥
--
module Data.MonoidMap.Examples.MultiMap.Instances.MultiMap1 where

import Prelude

import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.MonoidMap.Examples.MultiMap.Class as Class

newtype MultiMap1 k v = MultiMap (Map k (Set v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => Class.MultiMap MultiMap1 k v where

    fromList = MultiMap . Map.fromList

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

    update k vs (MultiMap m) = MultiMap (Map.insert k vs m)

    insert k vs (MultiMap m) = MultiMap $
        Map.insert k (Map.findWithDefault Set.empty k m `Set.union` vs) m

    remove k vs (MultiMap m) = MultiMap $
        Map.insert k (Map.findWithDefault Set.empty k m `Set.difference` vs) m

    union (MultiMap m1) (MultiMap m2) = MultiMap $
        Map.unionWith Set.union m1 m2

    intersection (MultiMap m1) (MultiMap m2) = MultiMap $
        Map.intersectionWith Set.intersection m1 m2
