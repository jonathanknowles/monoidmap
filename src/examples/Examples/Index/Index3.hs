-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- A lawful implementation of 'Index', implemented in terms of 'Map' and
-- 'NESet'.
--
module Examples.Index.Index3 where

import Prelude hiding
    ( lookup )

import Data.Map.Strict
    ( Map )
import Data.Set.NonEmpty
    ( NESet )
import Examples.Index
    ( Index (..) )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet

newtype Index3 k v = Index (Map k (NESet v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => Index Index3 k v where

    empty = Index Map.empty

    fromList = Index . Map.fromListWith (<>) . fmap (fmap NESet.singleton)

    toList (Index m) = fmap NESet.toSet <$> Map.toList m

    null (Index m) = Map.null m

    nonNullKey k (Index m) = Map.member k m

    nonNullKeys (Index m) = Map.keysSet m

    nonNullKeyCount (Index m) = Map.size m

    lookup k (Index m) = maybe Set.empty NESet.toSet (Map.lookup k m)

    update k vs (Index m) =
        case NESet.nonEmptySet vs of
            Nothing -> Index (Map.delete k    m)
            Just zs -> Index (Map.insert k zs m)

    add k vs i@(Index m) =
        case NESet.nonEmptySet (lookup k i `Set.union` vs) of
            Nothing -> Index (Map.delete k    m)
            Just zs -> Index (Map.insert k zs m)

    remove k vs i@(Index m) =
        case NESet.nonEmptySet (lookup k i `Set.difference` vs) of
            Nothing -> Index (Map.delete k    m)
            Just zs -> Index (Map.insert k zs m)

    union (Index m1) (Index m2) = Index $
        Map.unionWith NESet.union m1 m2

    intersection (Index m1) (Index m2) = Index $
        Map.merge
            Map.dropMissing
            Map.dropMissing
            (Map.zipWithMaybeMatched mergeValues)
            m1
            m2
      where
        mergeValues :: Ord v => k -> NESet v -> NESet v -> Maybe (NESet v)
        mergeValues _k s1 s2 = NESet.nonEmptySet (NESet.intersection s1 s2)

    isSubIndexOf (Index m1) (Index m2) = Map.isSubmapOfBy NESet.isSubsetOf m1 m2
