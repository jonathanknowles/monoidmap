-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- An unlawful implementation of 'Index', implemented in terms of 'Map' and
-- 'Set'.
--
-- This implementation has several subtle bugs.
--
module Examples.Index.Index1 where

import Prelude hiding
    ( lookup )

import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Examples.Index
    ( Index (..) )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype Index1 k v = Index (Map k (Set v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => Index Index1 k v where

    empty = Index Map.empty

    fromList = Index . Map.fromListWith (<>) . fmap (fmap Set.singleton)

    toList (Index m) = Map.toList m

    null (Index m) = Map.null m

    nonNullKey k (Index m) = Map.member k m

    nonNullKeys (Index m) = Map.keysSet m

    nonNullKeyCount (Index m) = Map.size m

    lookup k (Index m) = Map.findWithDefault Set.empty k m

    update k vs (Index m) = Index (Map.insert k vs m)

    add k vs i@(Index m) = Index $
        Map.insert k (lookup k i `Set.union` vs) m

    remove k vs i@(Index m) = Index $
        Map.insert k (lookup k i `Set.difference` vs) m

    union (Index m1) (Index m2) = Index $
        Map.unionWith Set.union m1 m2

    intersection (Index m1) (Index m2) = Index $
        Map.intersectionWith Set.intersection m1 m2

    isSubIndexOf (Index m1) (Index m2) = Map.isSubmapOfBy Set.isSubsetOf m1 m2
