-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- An unlawful implementation of 'MultiMap', implemented in terms of 'Map' and
-- 'Set'.
--
-- This implementation has several subtle bugs.
--
module Examples.MultiMap.MultiMap1 where

import Prelude hiding
    ( lookup )

import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Examples.MultiMap.Class
    ( MultiMap (..) )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype MultiMap1 k v = MultiMap (Map k (Set v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => MultiMap MultiMap1 k v where

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
        Map.insert k (lookup k (MultiMap m) `Set.union` vs) m

    remove k vs (MultiMap m) = MultiMap $
        Map.insert k (lookup k (MultiMap m) `Set.difference` vs) m

    union (MultiMap m1) (MultiMap m2) = MultiMap $
        Map.unionWith Set.union m1 m2

    intersection (MultiMap m1) (MultiMap m2) = MultiMap $
        Map.intersectionWith Set.intersection m1 m2
