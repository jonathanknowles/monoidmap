-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- A __lawful__ implementation of 'MultiMap', implemented in terms of
-- 'MonoidMap' and 'Set'.
--
module Data.MonoidMap.Examples.MultiMap.Instances.MultiMap4 where

import Prelude

import Data.MonoidMap
    ( MonoidMap )
import Data.Set
    ( Set )

import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set
import qualified Data.MonoidMap.Examples.MultiMap.Class as Class

newtype MultiMap4 k v = MultiMap (MonoidMap k (Set v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => Class.MultiMap MultiMap4 k v where

    fromList = MultiMap . MonoidMap.fromListWith (<>)

    toList (MultiMap m) = MonoidMap.toList m

    empty = MultiMap MonoidMap.empty

    lookup k (MultiMap m) = MonoidMap.get k m

    null (MultiMap m) = MonoidMap.null m

    nonNull (MultiMap m) = MonoidMap.nonNull m

    nonNullKey k (MultiMap m) = MonoidMap.nonNullKey k m

    nonNullKeys (MultiMap m) = MonoidMap.nonNullKeys m

    nonNullCount (MultiMap m) = MonoidMap.nonNullCount m

    isSubmapOf (MultiMap m1) (MultiMap m2) = m1 `MonoidMap.isSubmapOf` m2

    update k vs (MultiMap m) =
        MultiMap (MonoidMap.set k vs m)

    insert k vs (MultiMap m) =
        MultiMap (MonoidMap.adjust (`Set.union` vs) k m)

    remove k vs (MultiMap m) =
        MultiMap (MonoidMap.adjust (`Set.difference` vs) k m)

    union (MultiMap m1) (MultiMap m2) =
        MultiMap (MonoidMap.union m1 m2)

    intersection (MultiMap m1) (MultiMap m2) =
        MultiMap (MonoidMap.intersection m1 m2)
