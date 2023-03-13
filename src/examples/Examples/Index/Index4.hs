-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- A lawful implementation of 'Index', implemented in terms of 'MonoidMap' and
-- 'Set'.
--
module Examples.Index.Index4 where

import Prelude hiding
    ( lookup )

import Data.Set
    ( Set )
import Data.Total.MonoidMap
    ( MonoidMap )
import Examples.Index
    ( Index (..) )

import qualified Data.Set as Set
import qualified Data.Total.MonoidMap as MonoidMap

newtype Index4 k v = Index (MonoidMap k (Set v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => Index Index4 k v where

    empty = Index MonoidMap.empty

    fromList = Index . MonoidMap.fromListWith (<>) . fmap (fmap Set.singleton)

    toList (Index m) = MonoidMap.toList m

    null (Index m) = MonoidMap.null m

    nonNullKey k (Index m) = MonoidMap.nonNullKey k m

    nonNullKeys (Index m) = MonoidMap.nonNullKeys m

    nonNullKeyCount (Index m) = MonoidMap.nonNullCount m

    lookup k (Index m) = MonoidMap.get k m

    update k vs (Index m) = Index (MonoidMap.set k vs m)

    add k vs (Index m) = Index (MonoidMap.adjust (`Set.union` vs) k m)

    remove k vs (Index m) = Index (MonoidMap.adjust (`Set.difference` vs) k m)

    union (Index m1) (Index m2) = Index (MonoidMap.lcm m1 m2)

    intersection (Index m1) (Index m2) = Index (MonoidMap.gcd m1 m2)

    isSubIndexOf (Index m1) (Index m2) = m1 `MonoidMap.isPrefixOf` m2
