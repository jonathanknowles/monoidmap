-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- A lawful implementation of 'Index', implemented in terms of 'Map' and 'Set'.
--
module Examples.Index.Index2 where

import Prelude hiding
    ( lookup )

import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Examples.Index
    ( Index (..) )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype Index2 k v = Index (Map k (Set v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => Index Index2 k v where

    empty = Index Map.empty

    fromList = Index . Map.fromListWith (<>) . fmap (fmap Set.singleton)

    toList (Index m) = Map.toList m

    null (Index m) = Map.null m

    nonNullKey k (Index m) = Map.member k m

    nonNullKeys (Index m) = Map.keysSet m

    nonNullKeyCount (Index m) = Map.size m

    lookup k (Index m) = Map.findWithDefault Set.empty k m

    update k vs (Index m)
        | Set.null vs = Index (Map.delete k    m)
        | otherwise   = Index (Map.insert k vs m)

    add k vs i@(Index m)
        | Set.null zs = Index (Map.delete k    m)
        | otherwise   = Index (Map.insert k zs m)
      where
        zs = lookup k i `Set.union` vs

    remove k vs i@(Index m)
        | Set.null zs = Index (Map.delete k    m)
        | otherwise   = Index (Map.insert k zs m)
      where
        zs = lookup k i `Set.difference` vs

    union (Index m1) (Index m2) = Index $
        Map.unionWith Set.union m1 m2

    intersection (Index m1) (Index m2) = Index $
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
            | otherwise    = Just s3
          where
            s3 = Set.intersection s1 s2

    isSubIndexOf (Index m1) (Index m2) = Map.isSubmapOfBy Set.isSubsetOf m1 m2
