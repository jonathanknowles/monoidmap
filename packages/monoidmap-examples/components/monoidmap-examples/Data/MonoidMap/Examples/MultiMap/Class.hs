-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'MultiMap' class, which models a total relation from unique
-- keys to sets of values.
--
module Data.MonoidMap.Examples.MultiMap.Class where

import Data.Set
    ( Set )
import Prelude hiding
    ( lookup )

-- | Models a total relation from unique keys to sets of values.
--
class (Eq (m k v), Ord k, Ord v) => MultiMap m k v where

    -- | Constructs a multimap from a list of key to value set mappings.
    --
    -- Removing empty sets from the input list does not affect the result:
    --
    -- > fromList ≡ fromList . filter ((/= Set.empty) . snd)
    --
    fromList :: [(k, Set v)] -> m k v

    -- | Converts a multimap to a list of key to value-set mappings.
    --
    -- Removing empty sets from the output list does not affect the result:
    --
    -- > toList ≡ filter ((/= Set.empty) . snd) . toList
    --
    -- The resulting list can be used to reconstruct the original multimap:
    --
    -- > fromList . toList ≡ id
    --
    toList :: m k v -> [(k, Set v)]

    -- | Constructs an empty multimap.
    --
    -- > empty ≡ fromList []
    --
    empty :: m k v

    -- | Returns the set of values associated with a given key.
    --
    -- > lookup k (fromList kvs) ≡ foldMap snd (filter ((== k) . fst) kvs)
    --
    lookup :: k -> m k v -> Set v

    -- | Indicates whether or not a multimap is empty.
    --
    -- > null m ≡ (∀ k. lookup k m == Set.empty)
    --
    null :: m k v -> Bool

    -- | Indicates whether or not a multimap is non-empty.
    --
    -- > nonNull m ≡ (∃ k. lookup k m /= Set.empty)
    --
    nonNull :: m k v -> Bool

    -- | Returns 'True' iff. the given key is associated with a non-empty set.
    --
    -- > nonNullKey k m ≡ (lookup k m /= Set.empty)
    --
    nonNullKey :: k -> m k v -> Bool

    -- | Returns the set of keys that are associated with non-empty sets.
    --
    -- > all (`nonNullKey` m) (nonNullKeys m)
    --
    nonNullKeys :: m k v -> Set k

    -- | Indicates how many keys are associated with non-empty sets.
    --
    -- > nonNullCount m ≡ Set.size (nonNullKeys m)
    --
    nonNullCount :: m k v -> Int

    -- | Indicates whether or not the first map is a sub-map of the second.
    --
    -- > m1 `isSubmapOf` m2 ≡ ∀ k. (lookup k m1 `Set.isSubsetOf` lookup k m2)
    --
    isSubmapOf :: m k v -> m k v -> Bool

    -- | Updates the set of values associated with a given key.
    --
    -- > lookup k1 (update k2 vs m) ≡
    -- >     if k1 == k2
    -- >     then vs
    -- >     else lookup k1 m
    --
    update :: k -> Set v -> m k v -> m k v

    -- | Inserts values into the set of values associated with a given key.
    --
    -- > lookup k1 (insert k2 vs m) ≡
    -- >     if k1 == k2
    -- >     then lookup k1 m `Set.union` vs
    -- >     else lookup k1 m
    --
    insert :: k -> Set v -> m k v -> m k v

    -- | Removes values from the set of values associated with a given key.
    --
    -- > lookup k1 (remove k2 vs m) ≡
    -- >     if k1 == k2
    -- >     then lookup k1 m `Set.difference` vs
    -- >     else lookup k1 m
    --
    remove :: k -> Set v -> m k v -> m k v

    -- | Computes the union of two multimaps.
    --
    -- Instances must satisfy the following properties:
    --
    -- __/Idempotence/__
    --
    -- > union m m ≡ m
    --
    -- __/Identity/__
    --
    -- > union empty m     ≡ m
    -- > union m     empty ≡ m
    --
    -- __/Commutativity/__
    --
    -- > union m1 m2 ≡ union m2 m1
    --
    -- __/Associativity/__
    --
    -- > union        m1 (union m2  m3) ≡
    -- > union (union m1        m2) m3
    --
    -- __/Containment/__
    --
    -- > m1 `isSubmapOf` union m1 m2
    -- > m2 `isSubmapOf` union m1 m2
    --
    -- __/Distributivity/__
    --
    -- > lookup k (union m1 m2) ≡ Set.union (lookup k m1)
    -- >                                    (lookup k m2)
    --
    union :: m k v -> m k v -> m k v

    -- | Computes the intersection of two multimaps.
    --
    -- Instances must satisfy the following properties:
    --
    -- __/Idempotence/__
    --
    -- > intersection m m ≡ m
    --
    -- __/Identity/__
    --
    -- > intersection empty m     ≡ empty
    -- > intersection m     empty ≡ empty
    --
    -- __/Commutativity/__
    --
    -- > intersection m1 m2 ≡ intersection m2 m1
    --
    -- __/Associativity/__
    --
    -- > intersection               m1 (intersection m2  m3) ≡
    -- > intersection (intersection m1               m2) m3
    --
    -- __/Containment/__
    --
    -- > intersection m1 m2 `isSubmapOf` m1
    -- > intersection m1 m2 `isSubmapOf` m2
    --
    -- __/Distributivity/__
    --
    -- > lookup k (intersection m1 m2) ≡ Set.intersection (lookup k m1)
    -- >                                                  (lookup k m2)
    --
    intersection :: m k v -> m k v -> m k v
