-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- A lawful implementation of 'MultiMap', implemented in terms of 'MonoidMap'
-- and 'Set'.
--
module Examples.MultiMap.Instances.MultiMap4 where

import Prelude hiding
    ( gcd, lcm, lookup )

import Data.Monoid.GCD
    ( GCDMonoid (gcd) )
import Data.Monoid.LCM
    ( LCMMonoid (lcm) )
import Data.Monoid.Monus
    ( Monus ((<\>)) )
import Data.Set
    ( Set )
import Data.Total.MonoidMap
    ( MonoidMap )

import qualified Data.Total.MonoidMap as MonoidMap
import qualified Examples.MultiMap.Class as Class

newtype MultiMap k v = MultiMap (MonoidMap k (Set v))
    deriving stock (Eq, Show)

instance (Ord k, Ord v) => Class.MultiMap MultiMap k v where

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

    update k vs (MultiMap m) = MultiMap (MonoidMap.set k vs m)

    insert k vs (MultiMap m) = MultiMap (MonoidMap.adjust (<> vs) k m)

    remove k vs (MultiMap m) = MultiMap (MonoidMap.adjust (<\> vs) k m)

    union (MultiMap m1) (MultiMap m2) = MultiMap (lcm m1 m2)

    intersection (MultiMap m1) (MultiMap m2) = MultiMap (gcd m1 m2)
